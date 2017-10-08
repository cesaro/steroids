
int main ()
{
   int x;
   int y;
   int *p;
   
   if (x)
      p = &x;
   else
      p = &y;
   *p = 123333;
   return *p;
}

/*

define i32 @main() #0 {
  %1 = alloca i32, align 4
  %i = alloca i32, align 4
  %x = alloca i32, align 4
  %p = alloca i32*, align 8
  store i32 0, i32* %1
  store i32 0, i32* %i, align 4
  br label %2

; <label>:2                                       ; preds = %13, %0
  %3 = load i32, i32* %i, align 4
  %4 = add nsw i32 %3, 1
  store i32 %4, i32* %i, align 4
  %5 = icmp ne i32 %3, 0
  br i1 %5, label %6, label %17
  ; BLA

; <label>:6                                       ; preds = %2
  %7 = load i32, i32* %i, align 4
  %8 = srem i32 %7, 2
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  store i32* %i, i32** %p, align 8
  br label %12

; <label>:11                                      ; preds = %6
  store i32* %x, i32** %p, align 8
  br label %12

; <label>:12                                      ; preds = %11, %10
  br label %13

; <label>:13                                      ; preds = %12
  %14 = load i32, i32* %i, align 4
  %15 = icmp slt i32 %14, 10
  %16 = zext i1 %15 to i32
  br label %2

; <label>:17                                      ; preds = %2
  ret i32 123
}

%p -> stack(p)
stack(p) -> {inval, stack(x), stack(i)}

*/
