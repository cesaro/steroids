
define i32 @main() {
  %x = alloca i32
  %y = alloca i32
  %z = select i1 undef, i32* %x, i32* %y
  ret i32 123
}

; %z can point to both stack(x) and stack(y)

