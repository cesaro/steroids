
int main ()
{
   int x;
   int y;
   int z;

   int *px = &x;
   int *py = &y;
   
   int **p;
   p = x ? &px : &py;

   **p = 123;

   // flow insensitivity means that this will flow backwards to the writing above
   px = &z;

   return 0;
}

/*
  %1 = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %z = alloca i32, align 4
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
  store i32* %z, i32** %px, align 8
  ret i32 0

; %px in {st(px)}
; %px in {st(py)}
; %px in {st(p)}
; st(px) -> {Inval, st(x)}
; st(py) -> {Inval, st(y)}
; %7 in {st(px), st(py)}
; st(p) -> {st(px), st(py)}
; ...
; %9 in {Inval, st(x), st(y), st(z)}

*/
