define i32 @main() {
  %1 = alloca i32*, align 8
  %2 = inttoptr i32 757760 to i32*
  store i32* %2, i32** %1, align 8
  %3 = alloca i32, align 4
  store i32 3, i32* %3, align 4
  %4 = alloca i32*, align 8
  store i32* %3, i32** %4, align 8
  %5 = load i32*, i32** %4, align 8
  %6 = load i32, i32* %5, align 4
  ret i32 %6
}
