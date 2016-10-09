
.text

# exported symbols
.globl _rt_start     # void _rt_start (int argc, char **argv)
.globl _rt_end	    # void _rt_end (void)

_rt_start :
	# 0. save all registers
	# 2. save rsp into rt.host_rsp
	# 3. rsp = rt.stackend
	# 4. jump into C, call _rt_main, with the right arguments!

	jmp _rt_main # FIXME for the time being

	# 5. execute the exit prologue
	jmp _rt_end

_rt_end :
	# 1. restore host stack: rsp = rt.host_rsp
	# 2. restore all host registers from the stack
	# 3. return to the host code

   call _rt_panic # for the time being
	ret

