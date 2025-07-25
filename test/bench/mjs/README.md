cve-2023-43338

The source code is modified to remove floating point operations (see `default.nix`). This should have no impact on results since the vulnerability involves no decimal values.

Data compression (cs_varint_xxx) was also stubbed out (see `stubs.diff`) as it would unnecessarily complicate things.
