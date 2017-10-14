
================
Pointer Analysis
================

This folder contains an inter-procedural, flow-insensitive points-to analysis.
This document describes the goals of the analysis, how to use it as well as how
it is implemented.

Task list:
x check #ifndefs
x can I implement the fix point by adding the uses to the work list instead of
  the next one? YES!
x implement subsumption check, NodeBase::add() returns null
x improve the work list to avoid inserting duplicates; deque + set
x annotated bitcode output
x Fixpoint need to evaluate a function, not a module!
x pta-dump file.ll
x test folder + Makefile changes
x implemnt branches, phi, select
x test with ints
x test ifs
x test loops
x implemnt gep, intoptr/bitcast

CONTINUAR AQUI:

x ya tengo la instrumentacion de ld/st funcionando; comprender lo que guarda
- probar que pasa con dos o tres bench de tabla 1 y con 1 o 2 de los debian
- si es necesario optimizarla un poco y asegurarme de que guarda "lo minimo";
  por ejemplo, el valor de la posicion memoria no lo necesito par data races (si
  para cutoff); tampoco necesito la verificacion oom

- CONTINUAR AQUI: implementar Fixpoint::type_contains_pointers
- inicializar alloca correctamente en eval_instr_alloca
- probar de nuevo con gep3.ll
- test gep, intoptr, bitcast
- test arrays, structs
- implement malloc
- test mallocs
- how to do realloc? free? which others?
- design support for function calls / rets


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
                  if ty == pointer: assert (suc(n) == {Inval})
                  if ty != pointer: assert (suc(n) == empty)
                  assert (val(v) is empty or {n})
                  set val(v) = {n}
load(ty, ptr)     if loaded ty != pointer, then nop;
                  else, for any n \in val(ptr) add suc(n) to val(v)
store(ty,v,ptr)   if stored type != pointer, then nop; else:
                  for each n != Inval, Nullptr \in val(ptr)
                     make n point to all nodes in val(v)
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


Design
======

- There are two kinds of llvm::Value's in LLVM: global constants (functions,
  global variables, null pointer, constant expressions) and Instructions.
- Inside of a function, all uses of an instraction are dominated by the
  Instruction definition, so the BFS search will necessarily find first the
  definition and then the use.
- However, that's not the case for global constants, which can be used
  anywhere in body of a function.
- For this reason the constructo of the State class needs to initialize the
  valuation and the memory graph with all global llvm values. 
