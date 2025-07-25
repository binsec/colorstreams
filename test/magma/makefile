CONFIG_BASE = config

XML = 001 002 006 012
PDF = 003 004 010 018 019
SSL = 001 002 009 NEW001 NEW002 NEW003 NEW004 NEW005 NEW006
TIF = 001 002 002_2 008 008_2
SQL = 018
PNG = 007
PHP = 011

ALL = $(addprefix XML, $(XML)) $(addprefix PDF, $(PDF)) $(addprefix SSL, $(SSL)) $(addprefix TIF, $(TIF)) $(addprefix SQL, $(SQL)) $(addprefix PNG, $(PNG)) $(addprefix PHP, $(PHP))

SNSCOUNT = SSL002 PDF019

ifdef FILTER_OUT
	ALL_ = $(filter-out $(FILTER_OUT),$(ALL))
	SNSCOUNT_ = $(filter-out $(FILTER_OUT),$(SNSCOUNT))
else
	ALL_ = $(ALL)
	SNSCOUNT_ = $(SNSCOUNT)
endif

ALL_BYTEDEP = $(ALL_:=.bytedep)
SNSCOUNT_TODO = $(SNSCOUNT_:=.snscount)

.PHONY: build bytedep all full snscount clean_full clean_bytedep clean_snscount list_full list_bytedep list_snscount

all: full snscount

full: $(ALL_)

bytedep: $(ALL_BYTEDEP)

snscount: $(SNSCOUNT_TODO)

ALL_CLEAN = $(ALL_:=.clean)

clean_full: $(ALL_CLEAN)

BYTEDEP_CLEAN = $(ALL_BYTEDEP:=.clean)

clean_bytedep: $(BYTEDEP_CLEAN)

SNSCOUNT_CLEAN = $(SNSCOUNT_TODO:=.clean)

clean_snscount: $(SNSCOUNT_CLEAN)

list_full:
	echo $(ALL_)

list_bytedep:
	echo $(ALL_)

list_snscount:
	echo $(SNSCOUNT_)

build:
	nix-build

include ../common/makefile.base
