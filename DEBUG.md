# DEBUG

## Display
```shell
$ /home/shyuan/GitHub/RIOT/dist/tools/pyterm/pyterm -p "/dev/ttyACM0" -b "115200"
```

## Debug command
```shell
# another terminal
$ cd /home/shyuan/GitHub/RIOT
$ BOARD=nrf52840dk make -C tests/bench_bpf_jit_coq WERROR=0 flash debug
```
## GDB command

```shell
(gdb) b main # Set a breakpoint at the beginning of main
(gdb) r # Runs the program until a breakpoint or error, which is exactly the entry-point of main function
(gdb) layout split # divides the window into two parts - one of them displaying the source code, the other one the corresponding assembly

(gdb) ni # next instruction
(gdb) p i # Prints the current value of the variable "i", where i could be registers, e.g. "$r0"
(gdb) x i # Prints the current value of the variable "i", where i could be registers, e.g. "$r0"
(gdb) bt # Prints a stack trace
(gdb) up # Goes up a level in the stack
(gdb) down # Goes down a level in the stack
(gdb) info registers # Displays the contents of general-purpose processor registers 

(gdb) set $pc=0x... # set pc to point to specified location, e.g. executable array
```


