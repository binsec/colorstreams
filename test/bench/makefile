CONFIG_BASE = config

#all programs
ALL =

#full benchmark analysis
FULL =

#control-flow hijack capability analysis
CFH =

#S&S + counting
SNSCOUNT =

#relaxed SE (only vulnerable function)
RELAX =

#concretization
CONCR =

CFLAGS = -g -fno-stack-protector -O0 -pie

.PHONY: build all full cfh snscount default list_all list_full list_cfh list_snscount list_relax relax list_concr concr test

default: all

include arith/makefile
include can/makefile
include cfi/makefile
include grub/makefile
include heartbleed/makefile
include koobe/makefile
include libjpeg/makefile
include libsndfile/makefile
include minesweeper/makefile
include mongoose/makefile
include motex/makefile
include newsome/makefile
include spray/makefile
include uafubi/makefile
include uboot/makefile
include faad2/makefile
include perl-dbi/makefile
include sdop/makefile
include xfpt/makefile
include mjs/makefile

wildcardize = $(wildcard $(join $(addsuffix config/,$(dir $1)),$(addsuffix .config,$(notdir $1))))
do_filter = $(filter-out $(join $(subst config/,,$(dir $(call wildcardize,$1))),$(basename $(notdir $(call wildcardize,$1)))),$2)

ifdef FILTER_OUT
	FULL_ = $(call do_filter,$(FILTER_OUT),$(FULL))
	CFH_ = $(call do_filter,$(FILTER_OUT),$(CFH))
	SNSCOUNT_ = $(call do_filter,$(FILTER_OUT),$(SNSCOUNT))
	RELAX_ = $(call do_filter,$(FILTER_OUT),$(RELAX))
	CONCR_ = $(call do_filter,$(FILTER_OUT),$(CONCR)))
else
	FULL_ = $(FULL)
	CFH_ = $(CFH)
	SNSCOUNT_ = $(SNSCOUNT)
	RELAX_ = $(RELAX)
	CONCR_ = $(CONCR)
endif

ALL_RUN = $(ALL:=.run)

FULL_TODO = $(FULL_)
CFH_TODO = $(CFH_:=.cfh)
SNSCOUNT_TODO = $(SNSCOUNT_:=.snscount)
RELAX_TODO = $(RELAX_:=.relax)
CONCR_TODO = $(CONCR_:=.concr)

full: $(FULL_TODO)

cfh: $(CFH_TODO)

snscount: $(SNSCOUNT_TODO)

relax: $(RELAX_TODO)

concr: $(CONCR_TODO)

all: full cfh snscount relax concr

build: $(ALL_RUN)

list_all:
	echo $(ALL)

list_full:
	echo $(FULL_)

list_cfh:
	echo $(CFH_)

list_snscount:
	echo $(SNSCOUNT_)

list_relax:
	echo $(RELAX_)

list_concr:
	echo $(CONCR_)

%.run:
	nix-shell --run "gcc -o $@ $(CFLAGS) $(@:run=c) -lpinstrio" ./shell.nix

include ../common/makefile.base

