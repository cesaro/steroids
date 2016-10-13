
# text section
.text

# exported symbols
.globl _rt_start  # void _rt_start (int argc, char **argv)
.globl _rt_end	   # void _rt_end (void)
.type  _rt_start, @function
.type  _rt_end,   @function

_rt_start :
   # at this point the parameters are like this:
   # argc = %edi (32 bits)
   # argv = %rsi (64 bits)
   # env  = %rdx (64 bits)

   #jmp _rt_main

   # push calle-save registers to the host's stack
   push %rbx
   push %rbp
   push %r12
   push %r13
   push %r14
   push %r15

   # save arguments temporarily in calle-save register
   mov  %rdi, %r13 # argc (we copy 64 instead of 32 bits)
   mov  %rsi, %r14 # argv
   mov  %rdx, %r15 # env

   # save the current stack pointer at rt.host_rsp using the function
   # _rt_save_host_rsp below (in ah.c)
   mov  %rsp, %rdi
   call _rt_save_host_rsp

   # set up a temporary stack starting from the upper limit of the guest memory
   # (gas treats any undefined symbol as external)
   # I don't really undestand why the memend variable needs to be accessed as an
   # offset of the instruction pointer, could it be because we are asking to
   # compile the entire library with -fPIC?
   movq memend(%rip), %rax
   andq $-16, %rax
   mov  %rax, %rsp

   # call function _rt_main with exactly the same parameters as this function
   mov  %r13, %rdi # argc
   mov  %r14, %rsi # argv
   mov  %r15, %rdx # env
	call _rt_mainn

	# save return value (exit code) and fall through the exit routine _rt_end
   mov  %rax, %rdi

_rt_end :
	# make sure we have a correctly aligned stack; save exit code in rbx
   andq $-16, %rsp
   mov  %rdi, %rbx
   
   # call the _c_end function, last thing executed before going back
   call _rt_c_end

   # restore the host stack
   call _rt_get_host_rsp
   mov  %rax, %rsp

   # save exit code in %rax and restore calle-save registers from the host's
   # stack
   mov %rbx, %rax
   pop %r15
   pop %r14
   pop %r13
   pop %r12
   pop %rbp
   pop %rbx

   # return to the host
   ret

# vim:syn=gas:
