
#ifndef __STID_VAREMIT_HH_
#define __STID_VAREMIT_HH_

#pragma push_macro ("DEBUG")

#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
// ExecutionEngine.h exports DEBUG

#pragma pop_macro ("DEBUG")

namespace stid {

class Varemit
{
public:
   Varemit (llvm::Module *m_, llvm::ExecutionEngine *ee_, void *dst = nullptr) :
      m (m_),
      ee (ee_),
      dest (dst),
      size (0)
      { }
   ~Varemit ();

   void emit ();
   void clear ();

   void  *move_dest () { void *ptr = dest; clear(); return ptr; }
   void  *get_dest () const { return dest; }
   size_t get_size () const { return size; }

protected:
   llvm::Module *m;
   llvm::ExecutionEngine *ee;
   void *dest;
   size_t size;

   void *emit_or_size (void *base);

   virtual bool is_wanted (const llvm::GlobalVariable *g) const = 0;
   virtual void emit_var (llvm::GlobalVariable *g, void *ptr) = 0;

   //unsigned get_type_store_size (llvm::Type *t) const;
};

} // namespace

#endif
