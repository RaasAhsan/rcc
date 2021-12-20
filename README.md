# rcc

C compiler, references https://port70.net/~nsz/c/c89/c89-draft.html

```
$ gcc -c simple.c

$ ld -o simple /usr/lib/aarch64-linux-gnu/crt1.o /usr/lib/aarch64-linux-gnu/crti.o simple.o -lc /usr/lib/aarch64-linux-gnu/crtn.o

$ nasm -felf64 hello.asm
$ ld hello.o
```

Calling conventions for amd64 https://khoury.neu.edu/home/ntuck/courses/2018/09/cs3650/amd64_asm.html
