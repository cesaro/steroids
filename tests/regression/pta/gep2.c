
int main ()
{
   int x[10];
   int *px;

   px = x + *px;

   return 0;
}

/*
  %1 = alloca i32, align 4
  %x = alloca [10 x i32], align 16
  %px = alloca i32*, align 8
  store i32 0, i32* %1
  %2 = getelementptr inbounds [10 x i32], [10 x i32]* %x, i32 0, i32 0
  %3 = load i32*, i32** %px, align 8
  %4 = load i32, i32* %3, align 4
  %5 = sext i32 %4 to i64
  %6 = getelementptr inbounds i32, i32* %2, i64 %5
  store i32* %6, i32** %px, align 8

; ...
; %px in {st(px)}
; %2 in {st(x)}
; %3 in {Inval, st(x)}
; %4 nop
; %6 in {st(x)}
; st(px) -> st(x), Inval

*/
