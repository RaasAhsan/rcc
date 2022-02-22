define i32 @main() {
  %1 = alloca i32, align 4
  store i32 3, i32* %1, align 4
  %2 = alloca i32*, align 8
  store i32* %1, i32** %2, align 8
  %3 = alloca i32, align 4
  %4 = load i32*, i32** %2, align 8
  %5 = ptrtoint i32* %4 to i32
  store i32 %5, i32* %3, align 4
  %6 = load i32, i32* %3, align 4
  ret i32 %6
}
