
define i32 @main() {
  %x = alloca i32
  %z = select i1 undef, i32* null, i32* %x

  %p = alloca i32*
  store i32* null, i32** %p
  %pp = load i32*, i32** %p
  %v = select i1 undef, i32* null, i32* %pp

  ret i32 123
}

; %z is in {stack(x), null}
; %p is in {stack(p)}
; stack(p) -> {inval, null}
; %pp is in {inval, null}
; %v is in {inval, null}

