; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

%struct.p = type { %struct.z, %struct.z }
%struct.z = type { i32, i32 }
%struct.point = type { i64, float }
%struct.color = type { %union.anon }
%union.anon = type { i32 }
%struct.point2 = type { i32, i32 }

@__const.main.w = private unnamed_addr constant %struct.p { %struct.z { i32 1, i32 2 }, %struct.z { i32 4, i32 5 } }, align 4
@p = common global %struct.point zeroinitializer, align 8

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @pog(i32 %0, i32 %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %3, align 4
  ret i32 %6
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.p*, align 8
  %3 = alloca %struct.p*, align 8
  %4 = alloca %struct.p, align 4
  %5 = alloca %struct.color, align 4
  %6 = alloca %struct.point*, align 8
  store i32 0, i32* %1, align 4
  %7 = bitcast %struct.p* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %7, i8* align 4 bitcast (%struct.p* @__const.main.w to i8*), i64 16, i1 false)
  %8 = call i32 @pog(i32 2, i32 3)
  %9 = load %struct.point*, %struct.point** %6, align 8
  %10 = getelementptr inbounds %struct.point, %struct.point* %9, i32 0, i32 1
  %11 = load float, float* %10, align 8
  %12 = fptosi float %11 to i32
  ret i32 %12
}

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @foo() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.point2, align 4
  %3 = load i32, i32* %1, align 4
  ret i32 %3
}

attributes #0 = { noinline nounwind optnone ssp uwtable "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nofree nosync nounwind willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 12, i32 1]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"branch-target-enforcement", i32 0}
!3 = !{i32 1, !"sign-return-address", i32 0}
!4 = !{i32 1, !"sign-return-address-all", i32 0}
!5 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{!"Apple clang version 13.0.0 (clang-1300.0.29.30)"}
