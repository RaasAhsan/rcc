# rcc

C compiler, references https://port70.net/~nsz/c/c89/c89-draft.html

```
$ gcc -c simple.c

$ ld -o simple /usr/lib/aarch64-linux-gnu/crt1.o /usr/lib/aarch64-linux-gnu/crti.o simple.o -lc /usr/lib/aarch64-linux-gnu/crtn.o

$ nasm -felf64 hello.asm
$ ld hello.o
```

### Reference
* https://wiki.osdev.org/Calling_Conventions
* System V ABI https://wiki.osdev.org/System_V_ABI
* C calling conventions https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf
* Linux system call reference https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md
* GNU AS directives https://ftp.gnu.org/old-gnu/Manuals/gas-2.9.1/html_chapter/as_7.html
