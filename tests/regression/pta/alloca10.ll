
; TEST: % fun

; Function Attrs: nounwind uwtable
define i32 @fun() {
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %p = alloca i32*, align 8
  %1 = load i32, i32* %x, align 4
  %2 = icmp ne i32 %1, 0
  br i1 %2, label %3, label %4

; <label>:3                                       ; preds = %0
  %xx = alloca i32, align 4
  store i32* %xx, i32** %p, align 8
  %a = add nsw i32 %1, 1
  br label %5

; <label>:4                                       ; preds = %0
  store i32* %y, i32** %p, align 8
  %b = add nsw i32 %1, 2
  br label %5

; <label>:5                                       ; preds = %4, %3
  %ab = phi i32 [%a, %3], [%b, %4]
  %6 = load i32*, i32** %p, align 8
  store i32 %ab, i32* %6, align 4
  ret i32 0
}
