
define i32 @main() {
  %x = alloca i32
  %y = alloca i32
  %p = alloca i32*
  %q = alloca i32*
  store i32* %x, i32** %p
  store i32* %y, i32** %q
  br i1 undef, label %.yes, label %.no

.yes:
  %pp = load i32*, i32** %p
  br label %.join

.no:
  %qq = load i32*, i32** %q
  br label %.join

.join:
  %z = phi i32* [ %pp, %.yes], [ %qq, %.no]
  ret i32 123
}

; %z can point to both stack(x) and stack(y)
