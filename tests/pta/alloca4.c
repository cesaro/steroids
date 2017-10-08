
int main ()
{
   int i;
   int x;
   int *p;
  
   for (i = 0; i < 10; i++)
   {
      if (i % 2 == 0)
         p = &i;
      else
         p = &x;
   }
   return 123;
}

/*

define i32 @main() #0 {
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %p = alloca i32*, align 8
  store i32 0, i32* %1
  %2 = load i32, i32* %x, align 4
  %3 = icmp ne i32 %2, 0
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  store i32* %x, i32** %p, align 8
  br label %6

; <label>:5                                       ; preds = %0
  store i32* %y, i32** %p, align 8
  br label %6

; <label>:6                                       ; preds = %5, %4
  %7 = load i32*, i32** %p, align 8
  store i32 123333, i32* %7, align 4
  %8 = load i32*, i32** %p, align 8
  %9 = load i32, i32* %8, align 4
  ret i32 %9
}

%p -> stack(p)
stack(p) -> {inval, stack(x), stack(y)}

*/
