; ModuleID = 'tests/pta/fixpoint1.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %px = alloca i32*, align 8
  %py = alloca i32*, align 8
  %p = alloca i32**, align 8
  store i32 0, i32* %1
  store i32* %x, i32** %px, align 8
  store i32* %y, i32** %py, align 8
  %2 = load i32, i32* %x, align 4
  %3 = icmp ne i32 %2, 0
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  br label %6

; <label>:5                                       ; preds = %0
  br label %6

; <label>:6                                       ; preds = %5, %4
  %7 = phi i32** [ %px, %4 ], [ %py, %5 ]
  store i32** %7, i32*** %p, align 8
  %8 = load i32**, i32*** %p, align 8
  %9 = load i32*, i32** %8, align 8
  store i32 123, i32* %9, align 4
  ret i32 0
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.7.1-svn253742-1~exp1 (branches/release_37) (based on LLVM 3.7.1)"}
