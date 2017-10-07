
================
Pointer Analysis
================

This folder contains an inter-procedural, flow-sensitive points-to analysis.
This document describes the goals of the analysis, how to use it as well as how
it is implemented.

Task list:
x check #ifndefs
x can I implement the fix point by adding the uses to the work list instead of
  the next one? YES!
x implement subsumption check, NodeBase::add() returns null
- improve the work list to avoid inserting duplicates
- implemnt the transformers


Memory Objects
==============

There is 1 node per:
- **Top**.
  Represents all possible memory objects in the graph
- **Invalid**.
  Represents an invalid pointer. An uninitialized pointer or one that points to
  no legal/allocated memory object.
- **Nullptr**.
  Represents the `null` pointer
- **Function**
  A pointer to a function.
- **GlobalVariable**
  A pointer to a global variable, initialized or not.
- **Alloca**
  A pointer to the memory space allocated in the stack by some function.
- **Malloc**
  A pointer to the memory space allocated a call to malloc(3) (FIXME others?).


Transformers
============

Full listing of instructions in <llvm/IR/Instruction.def>

assert:
- suc(Nullptr) = \empty
- suc(Invalid) = \empty



// Terminator Instructions
ret               FIXME
br                nop; flow to the 2 branches
switch (i,l)+     nop; flow to all branches
indirectbr        nop; flow to all branches
invoke            Unsupported
resume            Unsupported
//catchswitch       Unsupported v6
//catchret          Unsupported v6
//cleanupret        Unsupported v6
unreachable       nop

// Standard binary operators...
add               nop
fadd              nop
sub               nop
fsub              nop
mul               nop
fmul              nop
udiv              nop
sdiv              nop
fdiv              nop
urem              nop
srem              nop
frem              nop

// Logical operators (integer operands)
shl               nop
lshr              nop
ashr              nop
and               nop
or                nop
xor               nop

// Memory operators...
alloca(ty)        let m(v) = n
                  assert (suc(n) = {Inval})
                  assert (val(v) is empty or {n})
                  set val(v) = {n}
load(ty, addr)    if loaded ty != ptr, then nop;
                  else, for any n \in val(addr) add suc(n) to val(v)
store(ty,v,addr)  if stored type != ptr, then nop
                  else for each node != Inval \in val(addr) add outgoing edges to G
                  pointing to each of the nodes in suc(val(v))
getelementptr(ptr, i1, i2...)
                  add each node in val(ptr) to val(v)
fence             nop
cmpxchg           Unsupported
atomicrmw         Unsupported

// Cast operators ...
trunc .. to       nop
zext .. to        nop
sext .. to        nop
fptrunc .. to     nop
fpext .. to       nop
fptoui .. to      nop
fptosi .. to      nop
uitofp .. to      nop
sitofp .. to      nop
ptrtoint .. to    nop
inttoptr .. to    add Top to val(v)
bitcast .. to     if dst ty != ptr; then nop
                  if dst ty == ptr && orig ty != ptr, then val(v) u= {Top}
                  if dst ty == ptr && orig ty == ptr, then val(v) u= val(orig)
addrspacecast .. to Unsupported

// Other operators...
icmp              nop
fcmp              nop
phi               if type != ptr, then nop; else val(v) u= val(left) \cup val(right)
call              FIXME
select            if type != ptr, then nop; else val(v) u= val(left) \cup val(right)
va_arg            FIXME // if type != ptr, then nop; else val(v) u= nex-arg from function call
extractelement    Unsupported
insertelement     Unsupported
shufflevector     Unsupported
extractvalue      Unsupported
insertvalue       Unsupported
landingpad        Unsupported
//catchpad          Unsupported v6
//cleanuppad        Unsupported v6
