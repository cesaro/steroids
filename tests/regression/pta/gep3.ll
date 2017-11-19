; ModuleID = 'gep3.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.my = type { i32, i32* }

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %x = alloca [10 x i32], align 16
  %m = alloca %struct.my, align 8
  store i32 0, i32* %1
  %2 = getelementptr inbounds %struct.my, %struct.my* %m, i32 0, i32 0
  store i32 10, i32* %2, align 4
  %3 = getelementptr inbounds [10 x i32], [10 x i32]* %x, i32 0, i32 0
  %4 = getelementptr inbounds i32, i32* %3, i64 0
  %5 = getelementptr inbounds %struct.my, %struct.my* %m, i32 0, i32 1
  store i32* %4, i32** %5, align 8
  ret i32 0
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.7.1-svn253742-1~exp1 (branches/release_37) (based on LLVM 3.7.1)"}
