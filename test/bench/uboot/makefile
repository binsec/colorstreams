ALL += $(addprefix uboot/, cve-2019-14192 cve-2019-14202 cve-2022-30790 cve-2022-30790_2 cve-2022-30552)
FULL += $(addprefix uboot/, cve-2019-14192 cve-2019-14202 cve-2022-30790 cve-2022-30790_2 cve-2022-30552)
SNSCOUNT += uboot/cve-2022-30790
SNSCOUNT += uboot/cve-2022-30790_2
RELAX += uboot/cve-2019-14192 uboot/cve-2019-14202
CONCR += $(addprefix uboot/, cve-2019-14192 cve-2019-14202 cve-2022-30790 cve-2022-30790_2 cve-2022-30552)

uboot/cve-2019-14192.run:
	cd uboot && nix-build

uboot/cve-2019-14202.run: uboot/cve-2019-14192.run
	echo ""

#cve-2019-14192_no_abort.run:
#	rm -f $@ && nix-build && ln -s result-3/u-boot $@

uboot/cve-2022-30790.run:
	cd uboot && nix-build

uboot/cve-2022-30790_2.run: uboot/cve-2022-30790.run
	echo ""

uboot/cve-2022-30552.run: uboot/cve-2022-30790.run
	echo ""
