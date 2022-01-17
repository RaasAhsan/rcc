; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.example = type { i32, i32 }

@__const.main.s = private unnamed_addr constant %struct.example { i32 5, i32 4 }, align 4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @foo2(i32* %0, i32 %1) #0 {
  %3 = alloca i32*, align 8
  %4 = alloca i32, align 4
  store i32* %0, i32** %3, align 8
  store volatile i32 %1, i32* %4, align 4
  ret i32 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @foo(i32 %0, i32 %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %10 = load i32, i32* %3, align 4
  %11 = mul nsw i32 %10, 2
  %12 = load i32, i32* %4, align 4
  %13 = mul nsw i32 %12, 3
  %14 = add nsw i32 %11, %13
  store i32 %14, i32* %5, align 4
  %15 = load i32, i32* %3, align 4
  %16 = mul nsw i32 %15, 4
  store i32 %16, i32* %6, align 4
  %17 = load i32, i32* %4, align 4
  %18 = add nsw i32 %17, 3
  store i32 %18, i32* %7, align 4
  %19 = load i32, i32* %4, align 4
  %20 = add nsw i32 %19, 3
  store i32 %20, i32* %8, align 4
  %21 = load i32, i32* %5, align 4
  %22 = add nsw i32 %21, 2
  store i32 %22, i32* %9, align 4
  %23 = load i32, i32* %5, align 4
  %24 = load i32, i32* %6, align 4
  %25 = add nsw i32 %23, %24
  %26 = load i32, i32* %7, align 4
  %27 = add nsw i32 %25, %26
  ret i32 %27
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca %struct.example, align 4
  %4 = alloca i8*, align 8
  store i32 0, i32* %1, align 4
  store i32 4, i32* %2, align 4
  %5 = call i32 @foo2(i32* %2, i32 4)
  %6 = bitcast %struct.example* %3 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %6, i8* align 4 bitcast (%struct.example* @__const.main.s to i8*), i64 8, i1 false)
  store i8* inttoptr (i64 753664 to i8*), i8** %4, align 8
  ret i32 0
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics" }
attributes #1 = { argmemonly nofree nounwind willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"branch-target-enforcement", i32 0}
!2 = !{i32 1, !"sign-return-address", i32 0}
!3 = !{i32 1, !"sign-return-address-all", i32 0}
!4 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!5 = !{i32 7, !"uwtable", i32 1}
!6 = !{i32 7, !"frame-pointer", i32 1}
!7 = !{!"Debian clang version 13.0.1-++20220115064408+fc043d8a256b-1~exp1~20220115064446.59"}
