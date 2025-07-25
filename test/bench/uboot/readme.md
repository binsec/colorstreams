cve-2022-30790 + cve-2022-30552:
https://research.nccgroup.com/2022/06/16/updated-technical-advisory-and-proofs-of-concept-multiple-vulnerabilities-in-u-boot-cve-2022-30790-cve-2022-30552/

Assessment of CVE-2022-30790 is wrong: despite the ability to control metadata of the linked list keeping track of holes in the packet reconstructed from fragments, it is not possible to write outside of the allocated buffer since the write address is calculated as the (static) base of the buffer + some offset. Metadata ONLY dictates whether the write is authorized or not and there is a bounds check on the global buffer (offset + size < len).

However CVE-2022-30790 allows to control 2 bytes of memory (cve-2022-30790-2) (aligned to 8, .X.. or ..X.) by setting hole metadata as next = addr and prev = X or vice versa (actually both at once) and sending a second packet perfectly covering the first hole. The address range is limited due to metadata using 16 bit integers, but it may be enough on some embedded systems?

The second "wlen" sink in cve-2022-30790 also showcases cve-2022-30552.
