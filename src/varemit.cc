
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

#include "stid/varemit.hh"
#include "verbosity.h"
#include "misc.hh"

namespace stid {

void Varemit::emit ()
{
   // this will pre-compute the size of the necessary memory region, and
   // initialize the memory only if dest is not null
   size = (char*) emit_or_size (dest) - (char*) dest;

   // if dest is null, we allocate memory and emit the initializers to memory
   if (! dest)
   {
      dest = new char[size];
      size_t s = (char*) emit_or_size (dest) - (char*) dest;
      ASSERT (s == size);
      (void) s;
   }
}

Varemit::~Varemit ()
{
   if (dest) delete[] (char*) dest;
}

void Varemit::clear ()
{
   dest = nullptr;
   size = 0;
}

void *Varemit::emit_or_size (void *base)
{
   llvm::GlobalVariable *g;
   llvm::Type *t;

   char *ptr = (char*) base;
   bool emit = base != nullptr;
   const llvm::DataLayout &dl = ee->getDataLayout ();

   for (llvm::GlobalValue &gg : m->globals())
   {
      // filter out unwanted globals
      g = llvm::dyn_cast<llvm::GlobalVariable> (&gg);
      if (!g or !is_wanted (g)) continue;
      if (!g) continue;
      ASSERT (g->getInitializer());
      ASSERT (g->getType()->isPointerTy());
      ASSERT (! g->isDeclaration());

      // globals always have pointer type !!!
      t = g->getType()->getPointerElementType();

      //std::string s; print_value (g, s); SHOW (s.c_str(), "s"); s.clear();
      //SHOW (ptr, "p");
      //g->getType()->dump ();
      //SHOW (dl.getTypeStoreSize (t), "u");
      //SHOW (dl.getTypeAllocSize (t), "u");

      // align the pointer as requested by the symbol
      ptr = (char*) llvm::alignAddr (ptr, dl.getABITypeAlignment (t));

      // we emit the initial value only if we have been requested so
      if (emit) emit_var (g, ptr);

      // consume the size of the symbol
      ptr += dl.getTypeAllocSize (t);
   }

   return ptr;
}

void Varemit::emit_var (llvm::GlobalVariable *g, void *ptr)
{
   ee->InitializeMemory (g->getInitializer(), ptr);
}

#if 0
unsigned Varemit::get_type_store_size (llvm::Type *t) const
{
   const llvm::DataLayout *dl = ee->getDataLayout ();

   // float types are missing in this function

   // integer types
   if (llvm::isa<llvm::IntegerType> (t))
      return dl->getTypeStoreSize (t);

   // function types (can this happen ??)
   ASSERT (! llvm::isa<llvm::IntegerType>(t));

   // pointer types, easy
   if (llvm::isa<llvm::PointerType> (t))
      return dl->getPointerSize (t->getPointerAddressSpace());

   // struct types: offset of the last + size of the last
   SHOW (llvm::dyn_cast<llvm::StructType> (t), "p");
   if (auto tt = llvm::dyn_cast<llvm::StructType> (t))
   {
      const llvm::StructLayout *sl = dl->getStructLayout (tt);
      unsigned num = tt->getNumElements();
      if (num == 0) return 1; // every type needs to consume space?
      return
            sl->getElementOffset(num-1) +
            get_type_store_size (tt->getElementType (num-1));
   }

   // array types: num of elements times spacing for 1 element
   if (auto tt = llvm::dyn_cast<llvm::ArrayType> (t))
   {
      unsigned num = tt->getNumElements();
      unsigned esize = dl->getTypeAllocSize (tt->getElementType());
      return num * esize;
   }

   // vector types types: same code (but different type)
   if (auto tt = llvm::dyn_cast<llvm::VectorType> (t))
   {
      unsigned num = tt->getNumElements();
      unsigned esize = dl->getTypeAllocSize (tt->getElementType());
      return num * esize;
   }
   ASSERT (0);
   return 0;
}
#endif

} // namespace
