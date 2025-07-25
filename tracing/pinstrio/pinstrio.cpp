#include <iostream>
#include <fstream>
#include <list>
#include <string>
#include <sstream>
#include <map>
#include <cstdarg>
#include <csignal>
#include <cstdint>
#include "pin.H"
using std::cout;
using std::cerr;
using std::cin;
using std::endl;
using std::flush;
using std::ios;
using std::ofstream;
using std::string;
using std::list;
using std::pair;
using std::map;
using std::stringstream;

#define FUN_TRACE_ID 0
#define INS_TRACE_ID 1
#define SRC_TRACE_ID 2
#define SNK_TRACE_ID 3
#define ANS_TRACE_ID 4
#define TPL_TRACE_ID 5
#define BBL_TRACE_ID 6

KNOB<string> knob_main(KNOB_MODE_WRITEONCE, "pintool", "main", "", "Main function (default: trace everything).");
//KNOB<bool> knob_interactive(KNOB_MODE_WRITEONCE, "pintool", "it", "0", "Toggles interactive mode.");
KNOB<bool> knob_atf(KNOB_MODE_WRITEONCE, "pintool", "atf", "0", "Always trace functions.");
KNOB<bool> knob_start(KNOB_MODE_WRITEONCE, "pintool", "start", "0", "Immediately start tracing.");
KNOB<bool> knob_no_avx(KNOB_MODE_WRITEONCE, "pintool", "no-avx512", "0", "Disable AVX.");
KNOB<bool> knob_no_abort(KNOB_MODE_WRITEONCE, "pintool", "no-abort", "0", "Ignore aborts.");
KNOB<string> knob_out(KNOB_MODE_WRITEONCE, "pintool", "out", "", "Communicate through a named pipe instead of stdout.");
KNOB<string> knob_in(KNOB_MODE_WRITEONCE, "pintool", "in", "", "Accept inputs through a named pipe instead of stdin.");
KNOB<bool> knob_binary(KNOB_MODE_WRITEONCE, "pintool", "binary", "0", "Communicate instruction traces as binary data.");

bool go = false;

std::ostream *out = &cout;
std::istream *in = &cin;

enum class OpType
{
    _register_,
    _immediate_,
    _address_generator_,
    _memory_,
    _other_
};

class OpInfo
{
    public:
        bool R, W;
        OpType t;
        UINT32 idx;
        string static_pretty;

        OpInfo(bool R, bool W)
        {
            this->R = R;
            this->W = W;
            idx = 99999;
            t = OpType::_other_;
            stringstream ss;
            if(R)
                ss << "R";
            if(W)
                ss << "W";
            static_pretty = ss.str();
        }

        OpInfo(INS ins, UINT32 op)
        {
            R = INS_OperandRead(ins, op);
            W = INS_OperandWritten(ins, op);
            t = OpType::_other_;
            idx = op;
            stringstream ss;
            if(!INS_OperandIsImplicit(ins, op))
                ss << idx << ":";
            if(R)
                ss << "R";
            if(W)
                ss << "W";
            static_pretty = ss.str();
        }

        virtual string pp()
        {
            return static_pretty;
        }

        virtual string pp_static()
        {
            return static_pretty;
        }

        virtual bool set_addr(ADDRINT addr)
        {
            return false;
        }
};

string REG_to_str(REG reg, bool par)
{
    char sep1 = par ? '(' : '<';
    char sep2 = par ? ')' : '>';
    bool valid = REG_valid(reg);
    REG full = REG_FullRegName(reg);
    string name = valid ? REG_StringShort(reg) : "invalid";
    string fullname = valid ? REG_StringShort(full) : "invalid";
    UINT32 size = valid ? REG_Size(reg) : 0;
    UINT32 fullsize = valid ? REG_Size(full) : 0;
    stringstream ss;
    ss << "REG" << sep1 << name << ":" << fullname << sep2 << "[" << size << ":" << fullsize << "]";
    return ss.str();
}

class RegOpInfo : public OpInfo
{
    public:
        REG reg;

        RegOpInfo(REG reg, bool R, bool W) : OpInfo(R, W)
        {
            t = OpType::_register_;
            this->reg = reg;
            stringstream ss;
            ss << static_pretty << REG_to_str(reg, false);
            static_pretty = ss.str();
        }

        RegOpInfo(INS ins, UINT32 op) : OpInfo(ins, op)
        {
            t = OpType::_register_;
            reg = INS_OperandReg(ins, op);
            stringstream ss;
            ss << static_pretty << REG_to_str(reg, false);
            static_pretty = ss.str();
        }
};

class ImmOpInfo : public OpInfo
{
    public:
        UINT64 val;

        ImmOpInfo(INS ins, UINT32 op) : OpInfo(ins, op)
        {
            t = OpType::_immediate_;
            val = INS_OperandImmediate(ins, op);
            stringstream ss;
            ss << static_pretty << "IMM<" << val << ">";
            static_pretty = ss.str();
        }
};

class AddrGenOpInfo : public OpInfo
{
    public:
        REG base, index, seg;
        ADDRDELTA displacement;
        UINT32 scale;
        string static_pretty_addrgen;

        AddrGenOpInfo(INS ins, UINT32 op) : OpInfo(ins, op)
        {
            t = OpType::_address_generator_;
            base = INS_OperandMemoryBaseReg(ins, op);
            displacement = INS_OperandMemoryDisplacement(ins, op);
            index = INS_OperandMemoryIndexReg(ins, op);
            scale = INS_OperandMemoryScale(ins, op);
            seg = INS_OperandMemorySegmentReg(ins, op);

            stringstream ss;
            if(REG_valid(base))
            {
                ss << idx << ":";
                ss << "R" << REG_to_str(base, false);
                if(REG_valid(index) || REG_valid(seg))
                    ss << " ";
            }
            if(REG_valid(index))
            {
                ss << idx << ":";
                ss << "R" << REG_to_str(index, false);
                if(REG_valid(seg))
                    ss << " ";
            }
            if(REG_valid(seg))
            {
                ss << idx << ":";
                ss << "R" << REG_to_str(seg, false);
            }
            static_pretty_addrgen = ss.str();
        }

        string pp() override
        {
            return static_pretty_addrgen;
        }

        string pp_static() override
        {
            return static_pretty_addrgen;
        }
};

class MemOpInfo : public AddrGenOpInfo
{
    public:
        ADDRINT addr;
        USIZE size;
        string static_template;
        string static_pretty_mem;

        MemOpInfo(INS ins, int num, UINT32 op, UINT32 memop) : AddrGenOpInfo(ins, op)
        {
            t = OpType::_memory_;
            addr = 0;
            size = INS_MemoryOperandSize(ins, memop);
            stringstream ss;
            ss << "(";
            ss << REG_to_str(seg, false);
            ss << "+" << REG_to_str(base, false);
            ss << std::dec << "+" << displacement;
            ss << "+" << REG_to_str(index, false) << "*" << scale;
            ss << ")";
            ss << "[" << size << "]";
            static_pretty_mem = ss.str();
            stringstream ss_;
            ss_ << OpInfo::static_pretty << "MEM<$" << num << ">" << static_pretty_mem;
            static_template = ss_.str();
        }

        string pp() override
        {
            stringstream ss;
            ss << OpInfo::static_pretty << "MEM<0x" << std::hex << addr << ">" << static_pretty_mem;
            return ss.str();
        }

        string pp_static() override
        {
            return static_template;
        }
        
        bool set_addr(ADDRINT addr) override
        {
            this->addr = addr;
            return true;
        }
};

std::map<xed_reg_enum_t, REG> *xed_reg_conv = nullptr;

class InstrInfo
{
    public:
        string sinstr, fname;
        list<OpInfo *> ops;
        int nMemW, nMemR;
        ADDRINT address;
        UINT64 size;
        bool cf, predicated;
        char flags;

        InstrInfo(INS ins)
        {
            if(!xed_reg_conv)
            {
                xed_reg_conv = new std::map<xed_reg_enum_t, REG>();
                for(REG r = REG_FIRST; r < REG_LAST; r++)
                    (*xed_reg_conv)[INS_XedExactMapFromPinReg(r)] = r;
            }

            RTN func = INS_Rtn(ins);
            if(RTN_Valid(func))
                fname = RTN_Name(func);
            else
                fname = "*invalid*";
            sinstr = INS_Disassemble(ins);
            address = INS_Address(ins);
            size = INS_Size(ins);
            cf = INS_IsControlFlow(ins);
            predicated = INS_GetPredicate(ins) != PREDICATE_ALWAYS_TRUE;
            flags = (predicated << 3) | (cf << 1);
            nMemW = 0;
            nMemR = 0;
            UINT32 nMemOp = 0;
            if(!INS_IsNop(ins))
            {
                for(UINT32 op = 0; op < INS_OperandCount(ins); op ++)
                {
                    if(INS_OperandIsReg(ins, op) && !INS_OperandIsImplicit(ins, op))
                        ops.push_back(new RegOpInfo(ins, op));
                    if(INS_OperandIsImmediate(ins, op))
                        ops.push_back(new ImmOpInfo(ins, op));
                    if(INS_OperandIsAddressGenerator(ins, op))
                        ops.push_back(new AddrGenOpInfo(ins, op));
                    if(INS_OperandIsMemory(ins, op))
                    {
                        MemOpInfo *opi = NULL;
                        if(INS_OperandRead(ins, op))
                        {
                            opi = new MemOpInfo(ins, nMemR, op, nMemOp);
                            nMemR ++;
                        }
                        if(INS_OperandWritten(ins, op))
                        {
                            opi = new MemOpInfo(ins, nMemW + 2, op, nMemOp);
                            nMemW ++;
                        }
                        if(opi)
                        {
                            ops.push_back(opi);
                            nMemOp ++;
                        }
                    }
                }
                const xed_decoded_inst_t *xedd = INS_XedDec(ins);
                const xed_inst_t *xedi = xed_decoded_inst_inst(xedd);
                for(unsigned int i = 0; i < xed_inst_noperands(xedi); i++)
                {
                    const xed_operand_t *op = xed_inst_operand(xedi, i);
                    xed_operand_enum_t op_name;
                    xed_reg_enum_t xedr;
                    REG reg;
                    switch(xed_operand_operand_visibility(op))
                    {
                        case XED_OPVIS_IMPLICIT:
                        case XED_OPVIS_SUPPRESSED:
                            op_name = xed_operand_name(op);
                            if(xed_operand_is_register(op_name)
                                    || xed_operand_is_memory_addressing_register(op_name))
                            {
                                xedr = xed_decoded_inst_get_reg(xedd, op_name);
                                reg = (*xed_reg_conv)[xedr];
                                if(REG_valid(reg))
                                {
                                    switch(xed_decoded_inst_operand_action(xedd, i))
                                    {
                                        case XED_OPERAND_ACTION_R:
                                        case XED_OPERAND_ACTION_CR:
                                            ops.push_back(new RegOpInfo(reg, true, false));
                                            break;
                                        case XED_OPERAND_ACTION_W:
                                        case XED_OPERAND_ACTION_CW:
                                            ops.push_back(new RegOpInfo(reg, false, true));
                                            break;
                                        case XED_OPERAND_ACTION_RW:
                                        case XED_OPERAND_ACTION_CRW:
                                        case XED_OPERAND_ACTION_RCW:
                                            ops.push_back(new RegOpInfo(reg, true, true));
                                            break;
                                        default:
                                            break;
                                    }
                                }
                            }
                        default:
                            break;
                    }
                }
            }
        }

        void set_mem_addrs(ADDRINT read1, ADDRINT read2, ADDRINT write)
        {
            bool first_read = true;
            for(list<OpInfo *>::iterator it = ops.begin(); it != ops.end(); it ++)
            {
                if((*it)->t == OpType::_memory_)
                {
                    if((*it)->R)
                    {
                        if(first_read && (*it)->set_addr(read1))
                            first_read = false;
                        else
                            (*it)->set_addr(read2);
                    }
                    if((*it)->W)
                        (*it)->set_addr(write);
                }
            }
        }

        string pp()
        {
            stringstream ss;
            ss << fname << ";";
            ss << "0x" << std::hex << address << ";";
            for(unsigned int i = 0; i < size; i ++)
                ss << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) address)[i]));
            ss << ";" << sinstr << ";";
            if(predicated)
                ss << "P;";
            else
                ss << "NP;";
            for(list<OpInfo *>::iterator it = ops.begin(); it != ops.end(); it ++)
                ss << (*it)->pp() << " ";
            return ss.str();
        }

        string pp_static()
        {
            stringstream ss;
            ss << fname << ";";
            ss << "0x" << std::hex << address << ";";
            for(unsigned int i = 0; i < size; i ++)
                ss << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) address)[i]));
            ss << ";" << sinstr << ";";
            if(cf)
                ss << "B;";
            else
                ss << "NB;";
            if(predicated)
                ss << "P;";
            else
                ss << "NP;";
            for(list<OpInfo *>::iterator it = ops.begin(); it != ops.end(); it ++)
                ss << (*it)->pp_static() << " ";
            return ss.str();
        }

        IARGLIST make_arglist()
        {
            IARGLIST al = IARGLIST_Alloc();
            IARGLIST_AddArguments(al, IARG_PTR, this, IARG_END);
            if(nMemR > 0)
                IARGLIST_AddArguments(al, IARG_MEMORYREAD_EA, IARG_END);
            else
                IARGLIST_AddArguments(al, IARG_ADDRINT, 0, IARG_END);
            if(nMemR > 1)
                IARGLIST_AddArguments(al, IARG_MEMORYREAD2_EA, IARG_END);
            else
                IARGLIST_AddArguments(al, IARG_ADDRINT, 0, IARG_END);
            if(nMemW > 0)
                IARGLIST_AddArguments(al, IARG_MEMORYWRITE_EA, IARG_END);
            else
                IARGLIST_AddArguments(al, IARG_ADDRINT, 0, IARG_END);
            return al;
        }
};

class FunInfo
{
    public:
        string name;

        FunInfo(string name)
        {
            this->name = name;
        }
};

class ValInit
{
    public:
        string desc = "";

        virtual IARGLIST make_arglist()
        {
            return IARGLIST_Alloc();
        }
};

std::map<string, REG> regs;

void init_reg_map()
{
    regs["ah"] = REG_AH;
    regs["al"] = REG_AL;
    regs["rdi"] = REG_APPLICATION_BASE;
    regs["rdf"] = REG_APPLICATION_LAST;
    regs["ax"] = REG_AX;
    regs["bh"] = REG_BH;
    regs["bl"] = REG_BL;
    regs["bp"] = REG_BP;
    regs["bpl"] = REG_BPL;
    regs["buf_base0"] = REG_BUF_BASE0;
    regs["buf_base1"] = REG_BUF_BASE1;
    regs["buf_base2"] = REG_BUF_BASE2;
    regs["buf_base3"] = REG_BUF_BASE3;
    regs["buf_base4"] = REG_BUF_BASE4;
    regs["buf_base5"] = REG_BUF_BASE5;
    regs["buf_base6"] = REG_BUF_BASE6;
    regs["buf_base7"] = REG_BUF_BASE7;
    regs["buf_base8"] = REG_BUF_BASE8;
    regs["buf_base9"] = REG_BUF_BASE9;
    regs["buf_base9"] = REG_BUF_BASE_LAST;
    regs["buf_end0"] = REG_BUF_END0;
    regs["buf_end1"] = REG_BUF_END1;
    regs["buf_end2"] = REG_BUF_END2;
    regs["buf_end3"] = REG_BUF_END3;
    regs["buf_end4"] = REG_BUF_END4;
    regs["buf_end5"] = REG_BUF_END5;
    regs["buf_end6"] = REG_BUF_END6;
    regs["buf_end7"] = REG_BUF_END7;
    regs["buf_end8"] = REG_BUF_END8;
    regs["buf_end9"] = REG_BUF_END9;
    regs["buf_end9"] = REG_BUF_ENDLAST;
    regs["buf_end9"] = REG_BUF_LAST;
    regs["bx"] = REG_BX;
    regs["ch"] = REG_CH;
    regs["cl"] = REG_CL;
    regs["cr0"] = REG_CR0;
    regs["cr1"] = REG_CR1;
    regs["cr2"] = REG_CR2;
    regs["cr3"] = REG_CR3;
    regs["cr4"] = REG_CR4;
    regs["cr0"] = REG_CR_BASE;
    regs["cr4"] = REG_CR_LAST;
    regs["cx"] = REG_CX;
    regs["rdf"] = REG_DF_FLAG;
    regs["dh"] = REG_DH;
    regs["di"] = REG_DI;
    regs["dil"] = REG_DIL;
    regs["dl"] = REG_DL;
    regs["dr0"] = REG_DR0;
    regs["dr1"] = REG_DR1;
    regs["dr2"] = REG_DR2;
    regs["dr3"] = REG_DR3;
    regs["dr4"] = REG_DR4;
    regs["dr5"] = REG_DR5;
    regs["dr6"] = REG_DR6;
    regs["dr7"] = REG_DR7;
    regs["dr0"] = REG_DR_BASE;
    regs["dr7"] = REG_DR_LAST;
    regs["dx"] = REG_DX;
    regs["eax"] = REG_EAX;
    regs["ebp"] = REG_EBP;
    regs["ebx"] = REG_EBX;
    regs["ecx"] = REG_ECX;
    regs["edi"] = REG_EDI;
    regs["edx"] = REG_EDX;
    regs["eflags"] = REG_EFLAGS;
    regs["eip"] = REG_EIP;
    regs["esi"] = REG_ESI;
    regs["esp"] = REG_ESP;
    regs["xmm0"] = REG_FIRST_FP_REG;
    regs["flags"] = REG_FLAGS;
    regs["fpcw"] = REG_FPCW;
    regs["fpdp_off"] = REG_FPDP_OFF;
    regs["fpdp_sel"] = REG_FPDP_SEL;
    regs["fpip_off"] = REG_FPIP_OFF;
    regs["fpip_sel"] = REG_FPIP_SEL;
    regs["fpopcode"] = REG_FPOPCODE;
    regs["fpcw"] = REG_FPSTATUS_BASE;
    regs["fpdp_sel"] = REG_FPSTATUS_LAST;
    regs["fpcw"] = REG_FPST_BASE;
    regs["st7"] = REG_FPST_LAST;
    regs["fpsw"] = REG_FPSW;
    regs["fptag"] = REG_FPTAG;
    regs["rax"] = REG_GAX;
    regs["rbp"] = REG_GBP;
    regs["rbx"] = REG_GBX;
    regs["rcx"] = REG_GCX;
    regs["rdi"] = REG_GDI;
    regs["rdx"] = REG_GDX;
    regs["rflags"] = REG_GFLAGS;
    regs["rdi"] = REG_GR_BASE;
    regs["r15"] = REG_GR_LAST;
    regs["rsi"] = REG_GSI;
    regs["k0"] = REG_IMPLICIT_FULL_MASK;
    regs["inst_g0"] = REG_INST_BASE;
    regs["inst_g0"] = REG_INST_G0;
    regs["inst_g0d"] = REG_INST_G0D;
    regs["inst_g1"] = REG_INST_G1;
    regs["inst_g10"] = REG_INST_G10;
    regs["inst_g10d"] = REG_INST_G10D;
    regs["inst_g11"] = REG_INST_G11;
    regs["inst_g11d"] = REG_INST_G11D;
    regs["inst_g12"] = REG_INST_G12;
    regs["inst_g12d"] = REG_INST_G12D;
    regs["inst_g13"] = REG_INST_G13;
    regs["inst_g13d"] = REG_INST_G13D;
    regs["inst_g14"] = REG_INST_G14;
    regs["inst_g14d"] = REG_INST_G14D;
    regs["inst_g15"] = REG_INST_G15;
    regs["inst_g15d"] = REG_INST_G15D;
    regs["inst_g16"] = REG_INST_G16;
    regs["inst_g16d"] = REG_INST_G16D;
    regs["inst_g17"] = REG_INST_G17;
    regs["inst_g17d"] = REG_INST_G17D;
    regs["inst_g18"] = REG_INST_G18;
    regs["inst_g18d"] = REG_INST_G18D;
    regs["inst_g19"] = REG_INST_G19;
    regs["inst_g19d"] = REG_INST_G19D;
    regs["inst_g1d"] = REG_INST_G1D;
    regs["inst_g2"] = REG_INST_G2;
    regs["inst_g20"] = REG_INST_G20;
    regs["inst_g20d"] = REG_INST_G20D;
    regs["inst_g21"] = REG_INST_G21;
    regs["inst_g21d"] = REG_INST_G21D;
    regs["inst_g22"] = REG_INST_G22;
    regs["inst_g22d"] = REG_INST_G22D;
    regs["inst_g23"] = REG_INST_G23;
    regs["inst_g23d"] = REG_INST_G23D;
    regs["inst_g24"] = REG_INST_G24;
    regs["inst_g24d"] = REG_INST_G24D;
    regs["inst_g25"] = REG_INST_G25;
    regs["inst_g25d"] = REG_INST_G25D;
    regs["inst_g26"] = REG_INST_G26;
    regs["inst_g26d"] = REG_INST_G26D;
    regs["inst_g27"] = REG_INST_G27;
    regs["inst_g27d"] = REG_INST_G27D;
    regs["inst_g28"] = REG_INST_G28;
    regs["inst_g28d"] = REG_INST_G28D;
    regs["inst_g29"] = REG_INST_G29;
    regs["inst_g29d"] = REG_INST_G29D;
    regs["inst_g2d"] = REG_INST_G2D;
    regs["inst_g3"] = REG_INST_G3;
    regs["inst_g3d"] = REG_INST_G3D;
    regs["inst_g4"] = REG_INST_G4;
    regs["inst_g4d"] = REG_INST_G4D;
    regs["inst_g5"] = REG_INST_G5;
    regs["inst_g5d"] = REG_INST_G5D;
    regs["inst_g6"] = REG_INST_G6;
    regs["inst_g6d"] = REG_INST_G6D;
    regs["inst_g7"] = REG_INST_G7;
    regs["inst_g7d"] = REG_INST_G7D;
    regs["inst_g8"] = REG_INST_G8;
    regs["inst_g8d"] = REG_INST_G8D;
    regs["inst_g9"] = REG_INST_G9;
    regs["inst_g9d"] = REG_INST_G9D;
    regs["rip"] = REG_INST_PTR;
    regs["inst_g0"] = REG_INST_SCRATCH_BASE;
    regs["buf_end9"] = REG_INST_SCRATCH_LAST;
    regs["inst_g0"] = REG_INST_TOOL_FIRST;
    regs["inst_g29"] = REG_INST_TOOL_LAST;
    regs["*invalid*"] = REG_INVALID_;
    regs["ip"] = REG_IP;
    regs["k0"] = REG_K0;
    regs["k1"] = REG_K1;
    regs["k2"] = REG_K2;
    regs["k3"] = REG_K3;
    regs["k4"] = REG_K4;
    regs["k5"] = REG_K5;
    regs["k6"] = REG_K6;
    regs["k7"] = REG_K7;
    regs["k0"] = REG_K_BASE;
    regs["k7"] = REG_K_LAST;
    regs["seg_fs_val"] = REG_LAST_CONTEXT_REG;
    regs["ldtr"] = REG_LDTR;
    regs["rdi"] = REG_MACHINE_BASE;
    regs["tr7"] = REG_MACHINE_LAST;
    regs["mm0"] = REG_MM0;
    regs["mm1"] = REG_MM1;
    regs["mm2"] = REG_MM2;
    regs["mm3"] = REG_MM3;
    regs["mm4"] = REG_MM4;
    regs["mm5"] = REG_MM5;
    regs["mm6"] = REG_MM6;
    regs["mm7"] = REG_MM7;
    regs["mm0"] = REG_MM_BASE;
    regs["mm7"] = REG_MM_LAST;
    regs["mxcsr"] = REG_MXCSR;
    regs["mxcsrmask"] = REG_MXCSRMASK;
    regs["*none*"] = REG_NONE;
    regs["orig_rax"] = REG_ORIG_GAX;
    regs["orig_rax"] = REG_ORIG_RAX;
    regs["rdi"] = REG_PHYSICAL_INTEGER_BASE;
    regs["rip"] = REG_PHYSICAL_INTEGER_END;
    regs["pin_ah"] = REG_PIN_AH;
    regs["pin_al"] = REG_PIN_AL;
    regs["pin_avx_in_use"] = REG_PIN_AVX_IN_USE;
    regs["pin_ax"] = REG_PIN_AX;
    regs["seg_gs_val"] = REG_PIN_BASE;
    regs["pin_bh"] = REG_PIN_BH;
    regs["pin_bl"] = REG_PIN_BL;
    regs["pin_bp"] = REG_PIN_BP;
    regs["pin_bpl"] = REG_PIN_BPL;
    regs["pin_bridge_app_ip"] = REG_PIN_BRIDGE_APP_IP;
    regs["pin_bridge_marshalling_frame"] = REG_PIN_BRIDGE_MARSHALLING_FRAME;
    regs["pin_bridge_multi_memoryaccess_frame"] = REG_PIN_BRIDGE_MULTI_MEMORYACCESS_FRAME;
    regs["pin_bridge_multi_memoryaccess_sp"] = REG_PIN_BRIDGE_MULTI_MEMORYACCESS_SP;
    regs["pin_bridge_on_stack_context_frame"] = REG_PIN_BRIDGE_ON_STACK_CONTEXT_FRAME;
    regs["pin_bridge_on_stack_context_sp"] = REG_PIN_BRIDGE_ON_STACK_CONTEXT_SP;
    regs["pin_bridge_orig_sp"] = REG_PIN_BRIDGE_ORIG_SP;
    regs["pin_bridge_spill_area_context_frame"] = REG_PIN_BRIDGE_SPILL_AREA_CONTEXT_FRAME;
    regs["pin_bridge_spill_area_context_sp"] = REG_PIN_BRIDGE_SPILL_AREA_CONTEXT_SP;
    regs["pin_bridge_sp_before_align"] = REG_PIN_BRIDGE_SP_BEFORE_ALIGN;
    regs["pin_bridge_sp_before_call"] = REG_PIN_BRIDGE_SP_BEFORE_CALL;
    regs["pin_bridge_sp_before_marshalling_frame"] = REG_PIN_BRIDGE_SP_BEFORE_MARSHALLING_FRAME;
    regs["pin_bridge_trans_memory_callback_frame"] = REG_PIN_BRIDGE_TRANS_MEMORY_CALLBACK_FRAME;
    regs["pin_bridge_trans_memory_callback_sp"] = REG_PIN_BRIDGE_TRANS_MEMORY_CALLBACK_SP;
    regs["pin_bx"] = REG_PIN_BX;
    regs["pin_ch"] = REG_PIN_CH;
    regs["pin_cl"] = REG_PIN_CL;
    regs["pin_cx"] = REG_PIN_CX;
    regs["pin_df"] = REG_PIN_DF_FLAG;
    regs["pin_dh"] = REG_PIN_DH;
    regs["pin_di"] = REG_PIN_DI;
    regs["pin_dil"] = REG_PIN_DIL;
    regs["pin_dl"] = REG_PIN_DL;
    regs["pin_dx"] = REG_PIN_DX;
    regs["pin_eax"] = REG_PIN_EAX;
    regs["pin_ebp"] = REG_PIN_EBP;
    regs["pin_ebx"] = REG_PIN_EBX;
    regs["pin_ecx"] = REG_PIN_ECX;
    regs["pin_edi"] = REG_PIN_EDI;
    regs["pin_edx"] = REG_PIN_EDX;
    regs["pin_esi"] = REG_PIN_ESI;
    regs["pin_esp"] = REG_PIN_ESP;
    regs["pin_flags"] = REG_PIN_FLAGS;
    regs["flags_before_ac_clearing"] = REG_PIN_FLAGS_BEFORE_AC_CLEARING;
    regs["pin_rax"] = REG_PIN_GAX;
    regs["pin_rbp"] = REG_PIN_GBP;
    regs["pin_rbx"] = REG_PIN_GBX;
    regs["pin_rcx"] = REG_PIN_GCX;
    regs["pin_rdi"] = REG_PIN_GDI;
    regs["pin_rdx"] = REG_PIN_GDX;
    regs["pin_edi"] = REG_PIN_GR_BASE;
    regs["pin_spillptr"] = REG_PIN_GR_LAST;
    regs["pin_rsi"] = REG_PIN_GSI;
    regs["pin_indirreg"] = REG_PIN_INDIRREG;
    regs["inst_cond"] = REG_PIN_INST_COND;
    regs["preserved_predicate"] = REG_PIN_INST_PRESERVED_PREDICATE;
    regs["inst_t0"] = REG_PIN_INST_T0;
    regs["inst_t0d"] = REG_PIN_INST_T0D;
    regs["inst_t0l"] = REG_PIN_INST_T0L;
    regs["inst_t0w"] = REG_PIN_INST_T0W;
    regs["inst_t1"] = REG_PIN_INST_T1;
    regs["inst_t1d"] = REG_PIN_INST_T1D;
    regs["inst_t1l"] = REG_PIN_INST_T1L;
    regs["inst_t1w"] = REG_PIN_INST_T1W;
    regs["inst_t2"] = REG_PIN_INST_T2;
    regs["inst_t2d"] = REG_PIN_INST_T2D;
    regs["inst_t2l"] = REG_PIN_INST_T2L;
    regs["inst_t2w"] = REG_PIN_INST_T2W;
    regs["inst_t3"] = REG_PIN_INST_T3;
    regs["inst_t3d"] = REG_PIN_INST_T3D;
    regs["inst_t3l"] = REG_PIN_INST_T3L;
    regs["inst_t3w"] = REG_PIN_INST_T3W;
    regs["pin_ipreladdr"] = REG_PIN_IPRELADDR;
    regs["pin_k0"] = REG_PIN_K0;
    regs["pin_k1"] = REG_PIN_K1;
    regs["pin_k2"] = REG_PIN_K2;
    regs["pin_k3"] = REG_PIN_K3;
    regs["pin_k4"] = REG_PIN_K4;
    regs["pin_k5"] = REG_PIN_K5;
    regs["pin_k6"] = REG_PIN_K6;
    regs["pin_k7"] = REG_PIN_K7;
    regs["pin_k0"] = REG_PIN_K_BASE;
    regs["pin_k7"] = REG_PIN_K_LAST;
    regs["pin_k7"] = REG_PIN_LAST;
    regs["pin_mem_operand_rewrite"] = REG_PIN_MEM_OPERAND_REWRITE;
    regs["pin_multi_mem_access_and_rewrite_eumlation_info_frame"] = REG_PIN_MULTI_MEM_ACCESS_AND_REWRITE_EMULATION_INFO_FRAME;
    regs["pin_mxcsr"] = REG_PIN_MXCSR;
    regs["pin_operands_info_gen_sp"] = REG_PIN_OPERANDS_INFO_GEN_SP;
    regs["pin_operands_info_op0"] = REG_PIN_OPERANDS_INFO_OP0;
    regs["pin_operands_info_op1"] = REG_PIN_OPERANDS_INFO_OP1;
    regs["pin_operands_info_op2"] = REG_PIN_OPERANDS_INFO_OP2;
    regs["pin_operands_info_op3"] = REG_PIN_OPERANDS_INFO_OP3;
    regs["pin_r10"] = REG_PIN_R10;
    regs["pin_r10b"] = REG_PIN_R10B;
    regs["pin_r10d"] = REG_PIN_R10D;
    regs["pin_r10w"] = REG_PIN_R10W;
    regs["pin_r11"] = REG_PIN_R11;
    regs["pin_r11b"] = REG_PIN_R11B;
    regs["pin_r11d"] = REG_PIN_R11D;
    regs["pin_r11w"] = REG_PIN_R11W;
    regs["pin_r12"] = REG_PIN_R12;
    regs["pin_r12b"] = REG_PIN_R12B;
    regs["pin_r12d"] = REG_PIN_R12D;
    regs["pin_r12w"] = REG_PIN_R12W;
    regs["pin_r13"] = REG_PIN_R13;
    regs["pin_r13b"] = REG_PIN_R13B;
    regs["pin_r13d"] = REG_PIN_R13D;
    regs["pin_r13w"] = REG_PIN_R13W;
    regs["pin_r14"] = REG_PIN_R14;
    regs["pin_r14b"] = REG_PIN_R14B;
    regs["pin_r14d"] = REG_PIN_R14D;
    regs["pin_r14w"] = REG_PIN_R14W;
    regs["pin_r15"] = REG_PIN_R15;
    regs["pin_r15b"] = REG_PIN_R15B;
    regs["pin_r15d"] = REG_PIN_R15D;
    regs["pin_r15w"] = REG_PIN_R15W;
    regs["pin_r8"] = REG_PIN_R8;
    regs["pin_r8b"] = REG_PIN_R8B;
    regs["pin_r8d"] = REG_PIN_R8D;
    regs["pin_r8w"] = REG_PIN_R8W;
    regs["pin_r9"] = REG_PIN_R9;
    regs["pin_r9b"] = REG_PIN_R9B;
    regs["pin_r9d"] = REG_PIN_R9D;
    regs["pin_r9w"] = REG_PIN_R9W;
    regs["pin_rax"] = REG_PIN_RAX;
    regs["pin_rbp"] = REG_PIN_RBP;
    regs["pin_rbx"] = REG_PIN_RBX;
    regs["pin_rcx"] = REG_PIN_RCX;
    regs["pin_rdi"] = REG_PIN_RDI;
    regs["pin_rdx"] = REG_PIN_RDX;
    regs["pin_rsi"] = REG_PIN_RSI;
    regs["pin_stack_ptr"] = REG_PIN_RSP;
    regs["seg_fs_val"] = REG_PIN_SEG_FS_VAL;
    regs["seg_gs_val"] = REG_PIN_SEG_GS_VAL;
    regs["pin_si"] = REG_PIN_SI;
    regs["pin_sil"] = REG_PIN_SIL;
    regs["pin_sp"] = REG_PIN_SP;
    regs["pin_spillptr"] = REG_PIN_SPILLPTR;
    regs["pin_spl"] = REG_PIN_SPL;
    regs["pin_stack_ptr"] = REG_PIN_STACK_PTR;
    regs["pin_status_flags"] = REG_PIN_STATUS_FLAGS;
    regs["pin_syscall_next_pc"] = REG_PIN_SYSCALL_NEXT_PC;
    regs["pin_sysenter_resumeaddr"] = REG_PIN_SYSENTER_RESUMEADDR;
    regs["pin_t0"] = REG_PIN_T0;
    regs["pin_t0d"] = REG_PIN_T0D;
    regs["pin_t0l"] = REG_PIN_T0L;
    regs["pin_t0w"] = REG_PIN_T0W;
    regs["pin_t1"] = REG_PIN_T1;
    regs["pin_t1d"] = REG_PIN_T1D;
    regs["pin_t1l"] = REG_PIN_T1L;
    regs["pin_t1w"] = REG_PIN_T1W;
    regs["pin_t2"] = REG_PIN_T2;
    regs["pin_t2d"] = REG_PIN_T2D;
    regs["pin_t2l"] = REG_PIN_T2L;
    regs["pin_t2w"] = REG_PIN_T2W;
    regs["pin_t3"] = REG_PIN_T3;
    regs["pin_t3d"] = REG_PIN_T3D;
    regs["pin_t3l"] = REG_PIN_T3L;
    regs["pin_t3w"] = REG_PIN_T3W;
    regs["pin_threadid"] = REG_PIN_THREAD_ID;
    regs["pin_tracked_tilecfg"] = REG_PIN_TRACKED_TILECFG;
    regs["pin_trans_memory_callback_read2_addr"] = REG_PIN_TRANS_MEMORY_CALLBACK_READ2_ADDR;
    regs["pin_trans_memory_callback_read_addr"] = REG_PIN_TRANS_MEMORY_CALLBACK_READ_ADDR;
    regs["pin_trans_memory_callback_write_addr"] = REG_PIN_TRANS_MEMORY_CALLBACK_WRITE_ADDR;
    regs["pin_t0"] = REG_PIN_T_BASE;
    regs["pin_t3l"] = REG_PIN_T_LAST;
    regs["pin_vmenter"] = REG_PIN_VMENTER;
    regs["pin_x87"] = REG_PIN_X87;
    regs["pin_xmm0"] = REG_PIN_XMM0;
    regs["pin_xmm1"] = REG_PIN_XMM1;
    regs["pin_xmm10"] = REG_PIN_XMM10;
    regs["pin_xmm11"] = REG_PIN_XMM11;
    regs["pin_xmm12"] = REG_PIN_XMM12;
    regs["pin_xmm13"] = REG_PIN_XMM13;
    regs["pin_xmm14"] = REG_PIN_XMM14;
    regs["pin_xmm15"] = REG_PIN_XMM15;
    regs["pin_xmm16"] = REG_PIN_XMM16;
    regs["pin_xmm17"] = REG_PIN_XMM17;
    regs["pin_xmm18"] = REG_PIN_XMM18;
    regs["pin_xmm19"] = REG_PIN_XMM19;
    regs["pin_xmm2"] = REG_PIN_XMM2;
    regs["pin_xmm20"] = REG_PIN_XMM20;
    regs["pin_xmm21"] = REG_PIN_XMM21;
    regs["pin_xmm22"] = REG_PIN_XMM22;
    regs["pin_xmm23"] = REG_PIN_XMM23;
    regs["pin_xmm24"] = REG_PIN_XMM24;
    regs["pin_xmm25"] = REG_PIN_XMM25;
    regs["pin_xmm26"] = REG_PIN_XMM26;
    regs["pin_xmm27"] = REG_PIN_XMM27;
    regs["pin_xmm28"] = REG_PIN_XMM28;
    regs["pin_xmm29"] = REG_PIN_XMM29;
    regs["pin_xmm3"] = REG_PIN_XMM3;
    regs["pin_xmm30"] = REG_PIN_XMM30;
    regs["pin_xmm31"] = REG_PIN_XMM31;
    regs["pin_xmm4"] = REG_PIN_XMM4;
    regs["pin_xmm5"] = REG_PIN_XMM5;
    regs["pin_xmm6"] = REG_PIN_XMM6;
    regs["pin_xmm7"] = REG_PIN_XMM7;
    regs["pin_xmm8"] = REG_PIN_XMM8;
    regs["pin_xmm9"] = REG_PIN_XMM9;
    regs["pin_xmm16"] = REG_PIN_XMM_AVX512_HI16_FIRST;
    regs["pin_xmm31"] = REG_PIN_XMM_AVX512_HI16_LAST;
    regs["pin_xmm31"] = REG_PIN_XMM_AVX512_LAST;
    regs["pin_xmm15"] = REG_PIN_XMM_AVX_LAST;
    regs["pin_xmm0"] = REG_PIN_XMM_BASE;
    regs["pin_xmm31"] = REG_PIN_XMM_LAST;
    regs["pin_xmm15"] = REG_PIN_XMM_SSE_LAST;
    regs["pin_ymm0"] = REG_PIN_YMM0;
    regs["pin_ymm1"] = REG_PIN_YMM1;
    regs["pin_ymm10"] = REG_PIN_YMM10;
    regs["pin_ymm11"] = REG_PIN_YMM11;
    regs["pin_ymm12"] = REG_PIN_YMM12;
    regs["pin_ymm13"] = REG_PIN_YMM13;
    regs["pin_ymm14"] = REG_PIN_YMM14;
    regs["pin_ymm15"] = REG_PIN_YMM15;
    regs["pin_ymm16"] = REG_PIN_YMM16;
    regs["pin_ymm17"] = REG_PIN_YMM17;
    regs["pin_ymm18"] = REG_PIN_YMM18;
    regs["pin_ymm19"] = REG_PIN_YMM19;
    regs["pin_ymm2"] = REG_PIN_YMM2;
    regs["pin_ymm20"] = REG_PIN_YMM20;
    regs["pin_ymm21"] = REG_PIN_YMM21;
    regs["pin_ymm22"] = REG_PIN_YMM22;
    regs["pin_ymm23"] = REG_PIN_YMM23;
    regs["pin_ymm24"] = REG_PIN_YMM24;
    regs["pin_ymm25"] = REG_PIN_YMM25;
    regs["pin_ymm26"] = REG_PIN_YMM26;
    regs["pin_ymm27"] = REG_PIN_YMM27;
    regs["pin_ymm28"] = REG_PIN_YMM28;
    regs["pin_ymm29"] = REG_PIN_YMM29;
    regs["pin_ymm3"] = REG_PIN_YMM3;
    regs["pin_ymm30"] = REG_PIN_YMM30;
    regs["pin_ymm31"] = REG_PIN_YMM31;
    regs["pin_ymm4"] = REG_PIN_YMM4;
    regs["pin_ymm5"] = REG_PIN_YMM5;
    regs["pin_ymm6"] = REG_PIN_YMM6;
    regs["pin_ymm7"] = REG_PIN_YMM7;
    regs["pin_ymm8"] = REG_PIN_YMM8;
    regs["pin_ymm9"] = REG_PIN_YMM9;
    regs["pin_ymm16"] = REG_PIN_YMM_AVX512_HI16_FIRST;
    regs["pin_ymm31"] = REG_PIN_YMM_AVX512_HI16_LAST;
    regs["pin_ymm31"] = REG_PIN_YMM_AVX512_LAST;
    regs["pin_ymm15"] = REG_PIN_YMM_AVX_LAST;
    regs["pin_ymm0"] = REG_PIN_YMM_BASE;
    regs["pin_ymm31"] = REG_PIN_YMM_LAST;
    regs["pin_zmm0"] = REG_PIN_ZMM0;
    regs["pin_zmm1"] = REG_PIN_ZMM1;
    regs["pin_zmm10"] = REG_PIN_ZMM10;
    regs["pin_zmm11"] = REG_PIN_ZMM11;
    regs["pin_zmm12"] = REG_PIN_ZMM12;
    regs["pin_zmm13"] = REG_PIN_ZMM13;
    regs["pin_zmm14"] = REG_PIN_ZMM14;
    regs["pin_zmm15"] = REG_PIN_ZMM15;
    regs["pin_zmm16"] = REG_PIN_ZMM16;
    regs["pin_zmm17"] = REG_PIN_ZMM17;
    regs["pin_zmm18"] = REG_PIN_ZMM18;
    regs["pin_zmm19"] = REG_PIN_ZMM19;
    regs["pin_zmm2"] = REG_PIN_ZMM2;
    regs["pin_zmm20"] = REG_PIN_ZMM20;
    regs["pin_zmm21"] = REG_PIN_ZMM21;
    regs["pin_zmm22"] = REG_PIN_ZMM22;
    regs["pin_zmm23"] = REG_PIN_ZMM23;
    regs["pin_zmm24"] = REG_PIN_ZMM24;
    regs["pin_zmm25"] = REG_PIN_ZMM25;
    regs["pin_zmm26"] = REG_PIN_ZMM26;
    regs["pin_zmm27"] = REG_PIN_ZMM27;
    regs["pin_zmm28"] = REG_PIN_ZMM28;
    regs["pin_zmm29"] = REG_PIN_ZMM29;
    regs["pin_zmm3"] = REG_PIN_ZMM3;
    regs["pin_zmm30"] = REG_PIN_ZMM30;
    regs["pin_zmm31"] = REG_PIN_ZMM31;
    regs["pin_zmm4"] = REG_PIN_ZMM4;
    regs["pin_zmm5"] = REG_PIN_ZMM5;
    regs["pin_zmm6"] = REG_PIN_ZMM6;
    regs["pin_zmm7"] = REG_PIN_ZMM7;
    regs["pin_zmm8"] = REG_PIN_ZMM8;
    regs["pin_zmm9"] = REG_PIN_ZMM9;
    regs["pin_zmm16"] = REG_PIN_ZMM_AVX512_HI16_FIRST;
    regs["pin_zmm31"] = REG_PIN_ZMM_AVX512_HI16_LAST;
    regs["pin_zmm31"] = REG_PIN_ZMM_AVX512_LAST;
    regs["pin_zmm15"] = REG_PIN_ZMM_AVX512_SPLIT_LAST;
    regs["pin_zmm0"] = REG_PIN_ZMM_BASE;
    regs["pin_zmm31"] = REG_PIN_ZMM_LAST;
    regs["r10"] = REG_R10;
    regs["r10b"] = REG_R10B;
    regs["r10d"] = REG_R10D;
    regs["r10w"] = REG_R10W;
    regs["r11"] = REG_R11;
    regs["r11b"] = REG_R11B;
    regs["r11d"] = REG_R11D;
    regs["r11w"] = REG_R11W;
    regs["r12"] = REG_R12;
    regs["r12b"] = REG_R12B;
    regs["r12d"] = REG_R12D;
    regs["r12w"] = REG_R12W;
    regs["r13"] = REG_R13;
    regs["r13b"] = REG_R13B;
    regs["r13d"] = REG_R13D;
    regs["r13w"] = REG_R13W;
    regs["r14"] = REG_R14;
    regs["r14b"] = REG_R14B;
    regs["r14d"] = REG_R14D;
    regs["r14w"] = REG_R14W;
    regs["r15"] = REG_R15;
    regs["r15b"] = REG_R15B;
    regs["r15d"] = REG_R15D;
    regs["r15w"] = REG_R15W;
    regs["r8"] = REG_R8;
    regs["r8b"] = REG_R8B;
    regs["r8d"] = REG_R8D;
    regs["r8w"] = REG_R8W;
    regs["r9"] = REG_R9;
    regs["r9b"] = REG_R9B;
    regs["r9d"] = REG_R9D;
    regs["r9w"] = REG_R9W;
    regs["rax"] = REG_RAX;
    regs["rdi"] = REG_RBASE;
    regs["rbp"] = REG_RBP;
    regs["rbx"] = REG_RBX;
    regs["rcx"] = REG_RCX;
    regs["rdi"] = REG_RDI;
    regs["rdx"] = REG_RDX;
    regs["rflags"] = REG_RFLAGS;
    regs["rip"] = REG_RIP;
    regs["rsi"] = REG_RSI;
    regs["rsp"] = REG_RSP;
    regs["cs"] = REG_SEG_BASE;
    regs["cs"] = REG_SEG_CS;
    regs["ds"] = REG_SEG_DS;
    regs["es"] = REG_SEG_ES;
    regs["fs"] = REG_SEG_FS;
    regs["seg_fs_base"] = REG_SEG_FS_BASE;
    regs["gs"] = REG_SEG_GS;
    regs["seg_gs_base"] = REG_SEG_GS_BASE;
    regs["gs"] = REG_SEG_LAST;
    regs["ss"] = REG_SEG_SS;
    regs["si"] = REG_SI;
    regs["sil"] = REG_SIL;
    regs["sp"] = REG_SP;
    regs["x87"] = REG_SPECIAL_BASE;
    regs["x87"] = REG_SPECIAL_LAST;
    regs["spl"] = REG_SPL;
    regs["st0"] = REG_ST0;
    regs["st1"] = REG_ST1;
    regs["st2"] = REG_ST2;
    regs["st3"] = REG_ST3;
    regs["st4"] = REG_ST4;
    regs["st5"] = REG_ST5;
    regs["st6"] = REG_ST6;
    regs["st7"] = REG_ST7;
    regs["rsp"] = REG_STACK_PTR;
    regs["r_status_flags"] = REG_STATUS_FLAGS;
    regs["st0"] = REG_ST_BASE;
    regs["st7"] = REG_ST_LAST;
    regs["tileconfig"] = REG_TILECONFIG;
    regs["tmm0"] = REG_TMM0;
    regs["tmm1"] = REG_TMM1;
    regs["tmm2"] = REG_TMM2;
    regs["tmm3"] = REG_TMM3;
    regs["tmm4"] = REG_TMM4;
    regs["tmm5"] = REG_TMM5;
    regs["tmm6"] = REG_TMM6;
    regs["tmm7"] = REG_TMM7;
    regs["tmm0"] = REG_TMM_FIRST;
    regs["tmm7"] = REG_TMM_LAST;
    regs["seg_gs_base"] = REG_TOOL_BASE;
    regs["inst_g29d"] = REG_TOOL_LAST;
    regs["rdi"] = REG_TO_SPILL_BASE;
    regs["tr"] = REG_TR;
    regs["tr3"] = REG_TR3;
    regs["tr4"] = REG_TR4;
    regs["tr5"] = REG_TR5;
    regs["tr6"] = REG_TR6;
    regs["tr7"] = REG_TR7;
    regs["tr"] = REG_TR_BASE;
    regs["tr7"] = REG_TR_LAST;
    regs["tssr"] = REG_TSSR;
    regs["x87"] = REG_X87;
    regs["xmm0"] = REG_XMM0;
    regs["xmm1"] = REG_XMM1;
    regs["xmm10"] = REG_XMM10;
    regs["xmm11"] = REG_XMM11;
    regs["xmm12"] = REG_XMM12;
    regs["xmm13"] = REG_XMM13;
    regs["xmm14"] = REG_XMM14;
    regs["xmm15"] = REG_XMM15;
    regs["xmm16"] = REG_XMM16;
    regs["xmm17"] = REG_XMM17;
    regs["xmm18"] = REG_XMM18;
    regs["xmm19"] = REG_XMM19;
    regs["xmm2"] = REG_XMM2;
    regs["xmm20"] = REG_XMM20;
    regs["xmm21"] = REG_XMM21;
    regs["xmm22"] = REG_XMM22;
    regs["xmm23"] = REG_XMM23;
    regs["xmm24"] = REG_XMM24;
    regs["xmm25"] = REG_XMM25;
    regs["xmm26"] = REG_XMM26;
    regs["xmm27"] = REG_XMM27;
    regs["xmm28"] = REG_XMM28;
    regs["xmm29"] = REG_XMM29;
    regs["xmm3"] = REG_XMM3;
    regs["xmm30"] = REG_XMM30;
    regs["xmm31"] = REG_XMM31;
    regs["xmm4"] = REG_XMM4;
    regs["xmm5"] = REG_XMM5;
    regs["xmm6"] = REG_XMM6;
    regs["xmm7"] = REG_XMM7;
    regs["xmm8"] = REG_XMM8;
    regs["xmm9"] = REG_XMM9;
    regs["xmm16"] = REG_XMM_AVX512_HI16_FIRST;
    regs["xmm31"] = REG_XMM_AVX512_HI16_LAST;
    regs["xmm31"] = REG_XMM_AVX512_LAST;
    regs["xmm15"] = REG_XMM_AVX_LAST;
    regs["xmm0"] = REG_XMM_BASE;
    regs["xmm31"] = REG_XMM_LAST;
    regs["xmm15"] = REG_XMM_SSE_LAST;
    regs["ymm0"] = REG_YMM0;
    regs["ymm1"] = REG_YMM1;
    regs["ymm10"] = REG_YMM10;
    regs["ymm11"] = REG_YMM11;
    regs["ymm12"] = REG_YMM12;
    regs["ymm13"] = REG_YMM13;
    regs["ymm14"] = REG_YMM14;
    regs["ymm15"] = REG_YMM15;
    regs["ymm16"] = REG_YMM16;
    regs["ymm17"] = REG_YMM17;
    regs["ymm18"] = REG_YMM18;
    regs["ymm19"] = REG_YMM19;
    regs["ymm2"] = REG_YMM2;
    regs["ymm20"] = REG_YMM20;
    regs["ymm21"] = REG_YMM21;
    regs["ymm22"] = REG_YMM22;
    regs["ymm23"] = REG_YMM23;
    regs["ymm24"] = REG_YMM24;
    regs["ymm25"] = REG_YMM25;
    regs["ymm26"] = REG_YMM26;
    regs["ymm27"] = REG_YMM27;
    regs["ymm28"] = REG_YMM28;
    regs["ymm29"] = REG_YMM29;
    regs["ymm3"] = REG_YMM3;
    regs["ymm30"] = REG_YMM30;
    regs["ymm31"] = REG_YMM31;
    regs["ymm4"] = REG_YMM4;
    regs["ymm5"] = REG_YMM5;
    regs["ymm6"] = REG_YMM6;
    regs["ymm7"] = REG_YMM7;
    regs["ymm8"] = REG_YMM8;
    regs["ymm9"] = REG_YMM9;
    regs["ymm16"] = REG_YMM_AVX512_HI16_FIRST;
    regs["ymm31"] = REG_YMM_AVX512_HI16_LAST;
    regs["ymm31"] = REG_YMM_AVX512_LAST;
    regs["ymm15"] = REG_YMM_AVX_LAST;
    regs["ymm0"] = REG_YMM_BASE;
    regs["ymm31"] = REG_YMM_LAST;
    regs["zmm0"] = REG_ZMM0;
    regs["zmm1"] = REG_ZMM1;
    regs["zmm10"] = REG_ZMM10;
    regs["zmm11"] = REG_ZMM11;
    regs["zmm12"] = REG_ZMM12;
    regs["zmm13"] = REG_ZMM13;
    regs["zmm14"] = REG_ZMM14;
    regs["zmm15"] = REG_ZMM15;
    regs["zmm16"] = REG_ZMM16;
    regs["zmm17"] = REG_ZMM17;
    regs["zmm18"] = REG_ZMM18;
    regs["zmm19"] = REG_ZMM19;
    regs["zmm2"] = REG_ZMM2;
    regs["zmm20"] = REG_ZMM20;
    regs["zmm21"] = REG_ZMM21;
    regs["zmm22"] = REG_ZMM22;
    regs["zmm23"] = REG_ZMM23;
    regs["zmm24"] = REG_ZMM24;
    regs["zmm25"] = REG_ZMM25;
    regs["zmm26"] = REG_ZMM26;
    regs["zmm27"] = REG_ZMM27;
    regs["zmm28"] = REG_ZMM28;
    regs["zmm29"] = REG_ZMM29;
    regs["zmm3"] = REG_ZMM3;
    regs["zmm30"] = REG_ZMM30;
    regs["zmm31"] = REG_ZMM31;
    regs["zmm4"] = REG_ZMM4;
    regs["zmm5"] = REG_ZMM5;
    regs["zmm6"] = REG_ZMM6;
    regs["zmm7"] = REG_ZMM7;
    regs["zmm8"] = REG_ZMM8;
    regs["zmm9"] = REG_ZMM9;
    regs["zmm16"] = REG_ZMM_AVX512_HI16_FIRST;
    regs["zmm31"] = REG_ZMM_AVX512_HI16_LAST;
    regs["zmm31"] = REG_ZMM_AVX512_LAST;
    regs["zmm15"] = REG_ZMM_AVX512_SPLIT_LAST;
    regs["zmm0"] = REG_ZMM_BASE;
    regs["zmm31"] = REG_ZMM_LAST;
}

void print_reg(CONTEXT *ctx, string sreg)
{
    auto it = regs.find(sreg);
    if(ctx && it != regs.end())
    {
        UINT32 size = REG_Size(it->second);
        UINT8 *val = (UINT8 *) malloc(size);
        PIN_GetContextRegval(ctx, it->second, val);
        *out << ANS_TRACE_ID;
        for(UINT32 i = 0; i < size; i++)
            *out << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(val[i])) << std::dec;
        *out << endl;
    }
    else
        *out << "ERROR" << endl;
}

void print_mem(CONTEXT *ctx, char *ptr, size_t s)
{
    *out << ANS_TRACE_ID;
    for(size_t j = 0; j < s; j++)
        *out << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(ptr[j])) << std::dec;
    *out << endl;
}

uint64_t stopat = 0;
bool reset_stopat = false;

bool parse(CONTEXT *ctx, InstrInfo *ii)
{
    string s;
    std::getline(*in, s);
    if(s.length())
    {
        stringstream ss;
        size_t i;
        uint64_t ptr;
        char c = s[0];
        switch(c)
        {
            case 'q':
                *out << ANS_TRACE_ID << "INTERRUPTED" << endl;
                exit(0);
            case 'v':
                if(ctx)
                {
                    if(s.length() > 2)
                        s = s.substr(2, s.length() - 2);
                    if(s.length() > 4 && s.substr(0, 4) == "REG<" && s[s.length() - 1] == '>')
                    {
                        s = s.substr(4, s.length() - 5);
                        print_reg(ctx, s);
                        return false;
                    }
                    i = s.find(">");
                    if(s.length() > 6 && s.substr(0, 6) == "MEM<0x" && i != std::string::npos)
                    {
                        ss << std::hex << s.substr(6, i - 6);
                        ss >> ptr;
                        i = s.find("[");
                        if(i != std::string::npos && s[s.length() - 1] && ']')
                        {
                            ss.clear();
                            ss.str("");
                            ss << std::dec << s.substr(i + 1, s.length() - i - 2);
                            ss >> i;
                            print_mem(ctx, (char *) ptr, i);
                            return false;
                        }
                    }
                }
                *out << ANS_TRACE_ID << "ERROR" << endl;
                return false;
            case 'p':
                *out << ANS_TRACE_ID << PIN_GetPid() << endl;
                return false;
            case 'd':
                PIN_ApplicationBreakpoint(ctx, PIN_ThreadId(), false, "pinstrio");
                return false;
            case 'g':
                if(s.length() > 4 && s.substr(1, 3) == " 0x")
                {
                    ss << std::hex << s.substr(4, s.length() - 4);
                    ss >> stopat;
                    reset_stopat = false;
                    return true;
                }
                *out << ANS_TRACE_ID << "ERROR" << endl;
                return false;
            case 't':
                if(ii)
                    *out << TPL_TRACE_ID << ii->pp_static() << endl;
                else
                    *out << ANS_TRACE_ID << "ERROR" << endl;
                return false;
        }
    }
    return true;
}

void interact(CONTEXT *ctx, InstrInfo *ii)
{
    //if(knob_interactive.Value())
        while(!parse(ctx, ii));
}

void interact(CONTEXT *ctx)
{
    //if(knob_interactive.Value())
        while(!parse(ctx, NULL));
}

void trace_bbl_bin(list<InstrInfo *> *bbl)
{
    if(!go)
        return;

    *out << BBL_TRACE_ID;
    size_t s = bbl->size();
    out->write((char *) &s, sizeof(size_t));
    for(auto it = bbl->begin(); it != bbl->end(); it++)
    {
        out->write((char *) &(*it)->address, sizeof((*it)->address));
        out->write((char *) &(*it)->size, sizeof((*it)->size));
        out->write((char *) (*it)->address, (*it)->size);
    }
    *out << endl;

    interact(NULL);
}

void trace_bbl(list<InstrInfo *> *bbl)
{
    if(!go)
        return;

    *out << BBL_TRACE_ID << bbl->size();
    for(auto it = bbl->begin(); it != bbl->end(); it++)
    {
        *out << " 0x" << std::hex << (*it)->address << ":";
        for(unsigned int i = 0; i < (*it)->size; i ++)
            *out << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) (*it)->address)[i]));
    }
    *out << endl;

    interact(NULL);
}

bool last_was_ret = false;

VOID trace_call(CONTEXT *ctx, UINT32 id, ADDRINT rsp)
{
    if(!go)
        return;
    *out << FUN_TRACE_ID << "Call(" << id << "," << rsp << ")" << endl;
    interact(ctx);
}

VOID trace_call_ret(CONTEXT *ctx, UINT32 id, ADDRINT rsp)
{
    if(!go || !last_was_ret)
        return;
    *out << FUN_TRACE_ID << "CallRet(" << id << "," << rsp << ")" << endl;
    interact(ctx);
}

typedef struct cpuid_inputs
{
    ADDRINT rax, rcx;
}
cpuid_inputs_t;

VOID check_cpuid(ADDRINT rax, ADDRINT rcx, cpuid_inputs_t *res)
{
    res->rax = rax;
    res->rcx = rcx;
}

VOID alter_cpuid(cpuid_inputs_t *inputs, UINT8 *rax, UINT8 *rbx, UINT8 *rcx, UINT8 *rdx)
{
    if(knob_no_avx.Value() && inputs->rax == 7)
    {
        if(inputs->rcx == 0)
        {
            ADDRINT *rbx_ = ((ADDRINT *) rbx);
            *rbx_ = *rbx_ & (~ 
                        (
                            (1 << 16) + //AVX512F
                            (1 << 17) + //AVX512DQ
                            (1 << 21) + //AVX512_IFMA
                            (1 << 26) + //AVX512PF
                            (1 << 27) + //AVX512ER
                            (1 << 28) + //AVX512CD
                            (1 << 30) + //AVX512BW
                            (1 << 31)   //AVX512VL
                        ));
            ADDRINT *rcx_ = ((ADDRINT *) rcx);
            *rcx_ = *rcx_ & (~
                        (
                            (1 << 1) +  //AVX512_VBMI
                            (1 << 6) +  //AVX512_VBMI2
                            (1 << 11) + //AVX512_VNNI
                            (1 << 12) + //AVX512_BITALG
                            (1 << 14)   //AVC512_VPOPCNTDQ
                        ));
            ADDRINT *rdx_ = ((ADDRINT *) rdx);
            *rdx_ = *rdx_ & (~
                        (
                            (1 << 2) +  //AVX512_4VNNIW
                            (1 << 3) +  //AVX512_4FMAPS
                            (1 << 8)    //AVX512_VP2INTERSECT
                        ));
        }
        if(inputs->rcx == 1)
        {
            ADDRINT *rax_ = ((ADDRINT *) rax);
            *rax_ = *rax_ & (~
                        (
                            (1 << 5)    //AVX512_BF16
                        ));
        }
    }
}

ADDRINT instr_check(ADDRINT addr, BOOL isret)
{
    stopat *= (!reset_stopat || addr == stopat);
    reset_stopat = addr == stopat;
    last_was_ret = isret;
    return go && addr == stopat;
}

VOID trace_instr_bin(CONTEXT *ctx, InstrInfo *ii, ADDRINT read1, ADDRINT read2, ADDRINT write, BOOL branch, BOOL exec)
{
    char flags = (exec << 2) | branch | ii->flags;
    *out << INS_TRACE_ID;
    out->write((char *) &ii->address, sizeof(ii->address));
    out->write((char *) &ii->size, sizeof(ii->size));
    out->write((char *) ii->address, ii->size);
    out->write(&flags, sizeof(flags));
    out->write((char *) &read1, sizeof(read1));
    out->write((char *) &read2, sizeof(read2));
    out->write((char *) &write, sizeof(write));
    *out << endl;
    interact(ctx, ii);
}

VOID trace_instr(CONTEXT *ctx, InstrInfo *ii, ADDRINT read1, ADDRINT read2, ADDRINT write, BOOL branch, BOOL exec)
{
    *out << INS_TRACE_ID;
    *out << std::hex << "0x" << ii->address << " ";
    for(unsigned int i = 0; i < ii->size; i ++)
        *out << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) ii->address)[i]));
    *out << " " << std::dec;
    if(ii->cf)
    {
        if(branch)
            *out << "B ";
        else
            *out << "NB ";
    }
    else 
        *out << "NA ";
    if(ii->predicated)
    {
        if(exec)
            *out << "E ";
        else
            *out << "NE ";
    }
    else
        *out << "NA ";
    *out << std::hex << "0x" << read1 << " " << "0x" << read2 << " " << "0x" << write << std::dec << endl;
    interact(ctx, ii);
}

VOID trace_fun_exit(CONTEXT *ctx, FunInfo *fi)
{
    if(!go && !knob_atf.Value())
        return;
    *out << FUN_TRACE_ID << "Exiting " << fi->name << endl;
    interact(ctx);
}

VOID stop()
{
    go = false;
}

int next_call_id = 0;
map<ADDRINT, int> call_ids;
map<ADDRINT, int> call_ret_ids;

VOID on_instr(INS ins, InstrInfo *ii) 
{
    if(INS_Opcode(ins) == XED_ICLASS_CPUID)
    {
        cpuid_inputs_t *inputs = (cpuid_inputs_t *) malloc(sizeof(cpuid_inputs_t));
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR) check_cpuid,
                IARG_REG_VALUE, REG_RAX,
                IARG_REG_VALUE, REG_RCX,
                IARG_PTR, inputs,
                IARG_END);
        INS_InsertCall(ins, IPOINT_AFTER, (AFUNPTR) alter_cpuid,
                IARG_PTR, inputs,
                IARG_REG_REFERENCE, REG_RAX,
                IARG_REG_REFERENCE, REG_RBX,
                IARG_REG_REFERENCE, REG_RCX,
                IARG_REG_REFERENCE, REG_RDX,
                IARG_END);
    }
    INS_InsertIfCall(ins, IPOINT_BEFORE, (AFUNPTR) instr_check,
            IARG_ADDRINT, ii->address,
            IARG_BOOL, INS_IsRet(ins),
            IARG_END);
    if(knob_binary.Value())
        INS_InsertThenCall(ins, IPOINT_BEFORE, (AFUNPTR) trace_instr_bin,
                IARG_CONST_CONTEXT,
                IARG_IARGLIST, ii->make_arglist(),
                IARG_BRANCH_TAKEN,
                IARG_EXECUTING,
                IARG_END);
    else
        INS_InsertThenCall(ins, IPOINT_BEFORE, (AFUNPTR) trace_instr,
                IARG_CONST_CONTEXT,
                IARG_IARGLIST, ii->make_arglist(),
                IARG_BRANCH_TAKEN,
                IARG_EXECUTING,
                IARG_END);
    if(INS_IsCall(ins))
    {
        if(call_ids.count(ii->address) == 0)
        {
            call_ids[ii->address] = next_call_id;
            call_ret_ids[INS_NextAddress(ins)] = next_call_id;
            next_call_id++;
        }
        INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR) trace_call,
                IARG_CONST_CONTEXT,
                IARG_UINT32, call_ids[ii->address],
                IARG_REG_VALUE, REG_RSP,
                IARG_END);
    }
    if(INS_IsRet(ins))
    {
        INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR) trace_fun_exit,
                IARG_CONST_CONTEXT,
                IARG_PTR, new FunInfo(ii->fname),
                IARG_END);
        if(ii->fname == knob_main.Value())
            INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR) stop, IARG_END);
    }
}

void on_trace(TRACE tr, VOID *v)
{
    for(BBL bbl = TRACE_BblHead(tr); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        INS ins = BBL_InsHead(bbl);
        list<InstrInfo *> *il = new list<InstrInfo *>();
        for(unsigned int i = 0; i < BBL_NumIns(bbl); i++)
        {
            InstrInfo *ii = new InstrInfo(ins);
            il->push_back(ii);
            ins = INS_Next(ins);
        }
        if(call_ret_ids.count(il->front()->address) > 0)
            BBL_InsertCall(bbl, IPOINT_BEFORE, (AFUNPTR) trace_call_ret,
                    IARG_CONST_CONTEXT,
                    IARG_UINT32, call_ret_ids[il->front()->address],
                    IARG_REG_VALUE, REG_RSP,
                    IARG_END);
        bool *first = (bool *) malloc(sizeof(bool));
        *first = true;
        if(knob_binary.Value())
            BBL_InsertCall(bbl, IPOINT_BEFORE, (AFUNPTR) trace_bbl_bin,
                    IARG_PTR, il,
                    IARG_PTR, first,
                    IARG_END);
        else
            BBL_InsertCall(bbl, IPOINT_BEFORE, (AFUNPTR) trace_bbl,
                    IARG_PTR, il,
                    IARG_PTR, first,
                    IARG_END);
        ins = BBL_InsHead(bbl);
        auto it = il->begin();
        for(unsigned int i = 0; i < BBL_NumIns(bbl); i++)
        {
            on_instr(ins, *it);
            ins = INS_Next(ins);
            it++;
        }
    }
}

VOID trace_fun_entry(CONTEXT *ctx, FunInfo *fi)
{
    if(!go && !knob_atf.Value())
        return;
    *out << FUN_TRACE_ID << "Entering " << fi->name << endl;
    interact(ctx);
}

VOID trace_source(CONTEXT *ctx, void *ptr, int size, char *tag)
{
    if(!go)
        return;
    *out << SRC_TRACE_ID << "Source(" << std::hex << ptr << std::dec;
    *out << ", " << size << ", " << tag << ")" << endl;
    interact(ctx);
}

int src_cnt = -1;

VOID trace_source_cnt(CONTEXT *ctx, void *ptr, int size, char *tag, int cnt)
{
    if(src_cnt > 0)
        src_cnt --;
    if(src_cnt == 0)
        trace_source(ctx, ptr, size, tag);
    if(src_cnt < 0)
        src_cnt = cnt;
}

VOID trace_sink(CONTEXT *ctx, void *ptr, int size, char *s)
{
    if(!go)
        return;
    *out << SNK_TRACE_ID << "Sink(" << std::hex << ptr << std::dec;
    *out << ", " << size << ", " << s;
    for(int i = 0; i < size; i ++)
        *out << ", " << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) ptr)[i])) << std::dec;
    *out << ")" << endl;
    interact(ctx);
}

VOID trace_constrained_sink(CONTEXT *ctx, void *ptr, int size, char *s, void *constr, int csize)
{
    if(!go)
        return;
    *out << SNK_TRACE_ID << "Sink(" << std::hex << ptr << std::dec;
    *out << ", " << size << ", " << s;
    for(int i = 0; i < size; i ++)
        *out << ", " << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) ptr)[i])) << std::dec;
    *out << ", " << std::hex << constr << std::dec << ", " << csize;
    for(int i = 0; i < csize; i ++)
        *out << ", " << std::hex << std::setfill('0') << std::setw(2) << (0xff & int(((char *) constr)[i])) << std::dec;
    *out << ")" << endl;
    interact(ctx);
}

bool first_start = true;

VOID start_common(CONTEXT *ctx)
{
    if(first_start)
    {
        first_start = false;
        if(PIN_GetDebugStatus() == DEBUG_STATUS_UNCONNECTED)
            *out << "Debugger failed to connect." << endl;
        else
            *out << "Ready to start!" << endl;
        interact(ctx);
    }
}

VOID start(CONTEXT *ctx)
{
    start_common(ctx);
    go = true;
}

VOID start_cnt(CONTEXT *ctx, int cnt, VOID *scnt_)
{
    int *scnt = (int *) scnt_;
    if(*scnt > 0)
        *scnt = *scnt - 1;
    if(*scnt < 0)
        *scnt = cnt;
    if(*scnt == 0)
    {
        start_common(ctx);
        go = true;
    }
}

VOID do_abort()
{
    if(!knob_no_abort.Value())
    {
        *out << ANS_TRACE_ID << "DONE" << endl;
        exit(0);
    }
}

VOID on_rtn(RTN rtn, VOID *v)
{
    RTN_Open(rtn);
    string fname = RTN_Name(rtn);
    if(fname == "_pinstrio_source_")
    {
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) trace_source,
                IARG_CONST_CONTEXT,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                IARG_END);
    }
    if(fname == "_pinstrio_source_cnt_")
    {
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) trace_source_cnt,
                IARG_CONST_CONTEXT,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                IARG_END);
    }
    if(fname == "_pinstrio_sink_")
    {
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) trace_sink,
                IARG_CONST_CONTEXT,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                IARG_END);
    }
    if(fname == "_pinstrio_constrained_sink_")
    {
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) trace_constrained_sink,
                IARG_CONST_CONTEXT,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 4,
                IARG_END);
    }
    if(fname == "_pinstrio_concrete_begin_")
        fname = "Concrete_begin";
    if(fname == "_pinstrio_concrete_end_")
        fname = "Concrete_end";
    if(fname == "_pinstrio_start_")
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) start, 
                IARG_CONST_CONTEXT,
                IARG_END);
    if(fname == "_pinstrio_start_cnt_")
    {
        int *cnt = (int *) malloc(sizeof(int));
        *cnt = -1;
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) start_cnt,
                IARG_CONST_CONTEXT,
                IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                IARG_PTR, cnt,
                IARG_END);
    }
    if(fname == "_pinstrio_stop_")
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) stop, IARG_END);
    if(fname == "_pinstrio_abort_")
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) do_abort, IARG_END);

    FunInfo *fi = new FunInfo(fname);
    if(fname == knob_main.Value())
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) start, 
                IARG_CONST_CONTEXT,
                IARG_END);
    RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR) trace_fun_entry,
            IARG_CONST_CONTEXT,
            IARG_PTR, fi,
            IARG_END);
    RTN_Close(rtn);
}

VOID on_fin(INT32 code, VOID *v)
{
    *out << ANS_TRACE_ID << "DONE" << endl;
}

BOOL on_sigsegv(THREADID tid, INT32 sig, CONTEXT *ctx, BOOL hashandler, const EXCEPTION_INFO *einfo, VOID *unused)
{
    *out << ANS_TRACE_ID << "SIGSEGV" << endl;
    return true;
}

BOOL dbg_on(THREADID id, CONTEXT *ctxt, const string &cmd , string *reply, VOID *v)
{
    if(cmd == "on")
    {
        go = true;
        return true;
    }
    return false;
}

BOOL dbg_off(THREADID id, CONTEXT *ctxt, const string &cmd , string *reply, VOID *v)
{
    if(cmd == "off")
    {
        go = false;
        return true;
    }
    return false;
}

INT32 usage()
{
    cerr << "This tool traces executed instructions and their inputs and outputs." << endl;
    cerr << endl << KNOB_BASE::StringKnobSummary() << endl;
    return -1;
}
 
int main(int argc, char* argv[])
{
    if (PIN_Init(argc, argv)) 
        return usage();

    init_reg_map();

    go = (knob_main.Value() == "" && knob_start.Value());

    if(knob_out.Value() != "")
        out = new std::fstream(knob_out.Value().c_str(), std::fstream::out);

    if(knob_in.Value() != "")
        in = new std::fstream(knob_in.Value().c_str(), std::fstream::in);
 
    RTN_AddInstrumentFunction(on_rtn, NULL);
    TRACE_AddInstrumentFunction(on_trace, NULL);

    PIN_AddFiniFunction(on_fin, 0);

    PIN_InterceptSignal(SIGSEGV, on_sigsegv, NULL);

    PIN_AddDebugInterpreter(dbg_on, NULL);
    PIN_AddDebugInterpreter(dbg_off, NULL);

    if(go)
        start_common(NULL);
 
    PIN_InitSymbols();

    PIN_StartProgram();
 
    return 0;
}
