
int main ()
{
   int x;
   int y;
   int *p, *q;
   p = &x;
   *p = 123;
   *q = 123333; // invalid storage
   return *p;
}

/*
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %p = alloca i32*, align 8
  %q = alloca i32*, align 8

  store i32 0, i32* %1
  ; nop

  store i32* %x, i32** %p, align 8
  ; %p = {Inval, x}

  %2 = load i32*, i32** %p, align 8
  ; %2 = {Inval, x}

  store i32 123, i32* %2, align 4
  %3 = load i32*, i32** %q, align 8
  store i32 123333, i32* %3, align 4
  %4 = load i32*, i32** %p, align 8
  %5 = load i32, i32* %4, align 4
  ret i32 %5
*/
