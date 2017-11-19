
struct in
{
   int *p;
   int a[2];
};

struct my
{
   int x;
   struct in *p;
   struct in a[2];
   struct my *next;
};

struct in i;
struct my m1 = {17, 0, {{0, {1, 2}}, {0, {1, 2}}}, 0};
struct my m2 = {17, &i, {{0, {1, 2}}, {0, {1, 2}}}, 0};
struct my m3 = {17, 0, {{&m3.x, {1, 2}}, {m3.a[1].a + 1, {1, 2}}}, 0};

int main ()
{
   struct my m = {17, 0, {{0, {1, 2}}, {0, {1, 2}}}, 0};
   printf ("%s\n", &m);
   return m.x;
}

/*
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

; ...
; %px in {st(px)}
; %2 in {st(m)}
; %3 in {st(x)}
; %4 in {st(x)}
; %5 in {st(m)}
; st(m) -> st(x), Inval

*/
