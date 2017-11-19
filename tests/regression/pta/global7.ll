; ModuleID = 'global7.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.my = type { i32, %struct.in*, [2 x %struct.in], %struct.my* }
%struct.in = type { i32*, [2 x i32] }

@m1 = global %struct.my { i32 17, %struct.in* null, [2 x %struct.in] [%struct.in { i32* null, [2 x i32] [i32 1, i32 2] }, %struct.in { i32* null, [2 x i32] [i32 1, i32 2] }], %struct.my* null }, align 8
@i = common global %struct.in zeroinitializer, align 8
@m2 = global %struct.my { i32 17, %struct.in* @i, [2 x %struct.in] [%struct.in { i32* null, [2 x i32] [i32 1, i32 2] }, %struct.in { i32* null, [2 x i32] [i32 1, i32 2] }], %struct.my* null }, align 8
@m3 = global %struct.my { i32 17, %struct.in* null, [2 x %struct.in] [%struct.in { i32* getelementptr inbounds (%struct.my, %struct.my* @m3, i32 0, i32 0), [2 x i32] [i32 1, i32 2] }, %struct.in { i32* bitcast (i8* getelementptr (i8, i8* bitcast (%struct.my* @m3 to i8*), i64 44) to i32*), [2 x i32] [i32 1, i32 2] }], %struct.my* null }, align 8
@.str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %m = alloca %struct.my, align 8
  store i32 0, i32* %1
  %2 = bitcast %struct.my* %m to i8*
  call void @llvm.memset.p0i8.i64(i8* %2, i8 0, i64 56, i32 8, i1 false)
  %3 = bitcast i8* %2 to %struct.my*
  %4 = getelementptr %struct.my, %struct.my* %3, i32 0, i32 0
  store i32 17, i32* %4
  %5 = getelementptr %struct.my, %struct.my* %3, i32 0, i32 2
  %6 = getelementptr [2 x %struct.in], [2 x %struct.in]* %5, i32 0, i32 0
  %7 = getelementptr %struct.in, %struct.in* %6, i32 0, i32 1
  %8 = getelementptr [2 x i32], [2 x i32]* %7, i32 0, i32 0
  store i32 1, i32* %8
  %9 = getelementptr [2 x i32], [2 x i32]* %7, i32 0, i32 1
  store i32 2, i32* %9
  %10 = getelementptr [2 x %struct.in], [2 x %struct.in]* %5, i32 0, i32 1
  %11 = getelementptr %struct.in, %struct.in* %10, i32 0, i32 1
  %12 = getelementptr [2 x i32], [2 x i32]* %11, i32 0, i32 0
  store i32 1, i32* %12
  %13 = getelementptr [2 x i32], [2 x i32]* %11, i32 0, i32 1
  store i32 2, i32* %13
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), %struct.my* %m)
  %15 = getelementptr inbounds %struct.my, %struct.my* %m, i32 0, i32 0
  %16 = load i32, i32* %15, align 4
  ret i32 %16
}

; Function Attrs: nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #1

declare i32 @printf(i8*, ...) #2

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }
attributes #2 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.7.1-svn253742-1~exp1 (branches/release_37) (based on LLVM 3.7.1)"}
