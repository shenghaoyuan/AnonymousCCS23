# CompCert-ebpf

## Install CompCert-ebpf

https://gitlab.inria.fr/vlafeych/CompCert

To compile:
./configure ebpf-riot
make

This requires  llvm-mc to perform the assembly phase.

I have updated the ebpf backend (branch wip).
./configure rbpf; make
This restrict the use of registers for division and immediate for shifts
./configure ebpf32; make
ebpf backend without the restrictions
