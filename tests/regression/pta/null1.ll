
define i32 @main() {
  %p = alloca i32*
  store i32* null, i32** %p
  ret i32 123
}

; %p is in {stack(p)}
; stack(p) -> {null, inval}

