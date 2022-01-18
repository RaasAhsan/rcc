define i32 @main() {
  %1 = alloca i32, align 4
  store i32 3, i32* %1, align 4
  %2 = alloca i32, align 4
  store i32 5, i32* %2, align 4
  %3 = alloca i32, align 4
  %4 = load i32, i32* %1, align 4
  %5 = load i32, i32* %2, align 4
  %6 = add i32 %4, %5
  store i32 %6, i32* %3, align 4
  %7 = load i32, i32* %3, align 4
  ret i32 %7
}
