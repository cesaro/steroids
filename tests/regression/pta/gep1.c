
int main ()
{
   int x[10];
   int *px;

   px = x + 5;

   return 0;
}

/*

  %1 = alloca i32, align 4
  %x = alloca [10 x i32], align 16
  %px = alloca i32*, align 8
  store i32 0, i32* %1
  %2 = getelementptr inbounds [10 x i32], [10 x i32]* %x, i32 0, i32 0
  %3 = getelementptr inbounds i32, i32* %2, i64 5
  store i32* %3, i32** %px, align 8
  ret i32 0

; ...
; %2 in {st(x)}
; %3 in {st(x)}
; st(px) -> st(x)

*/
