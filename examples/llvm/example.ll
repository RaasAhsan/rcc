define i32 @mul_add(i32 %x, i32 %y, i32 %z) {
entry:
  %tmp = mul i32 %x, %y
  %tmp2 = add i32 %tmp, %z
  ret i32 %tmp2
}

declare i32 @putchar(i32 %c)

define i32 @main() {
  %tmp = call i32 @putchar(i32 57)
  ret i32 10
}
