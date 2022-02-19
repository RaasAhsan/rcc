define i32 @main() {
  %1 = alloca i32, align 4
  store i32 3, i32* %1, align 4
  %2 = alloca i32*, align 8
  store i32* %1, i32** %2, align 8
  %4 = load i32*, i32** %2, align 8
  %5 = load i32, i32* %4, align 4
  ret i32 %5
}
