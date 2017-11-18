
define i32 @main() {
  %p = alloca i32*
  store i32* undef, i32** %p

  ret i32 123
}

; stack(p) -> {inval}


