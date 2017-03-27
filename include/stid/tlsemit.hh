
#ifndef __STID_TLSEMIT_HH_
#define __STID_TLSEMIT_HH_

#include "stid/varemit.hh"

namespace stid {

typedef std::unordered_map<llvm::GlobalVariable*,unsigned> Tlsoffsetmap;

class Tlsemit : public Varemit
{
public:
   using Varemit::Varemit;
   Tlsoffsetmap map;


private :
   bool is_wanted (const llvm::GlobalVariable *g) const final
   {
      return g->isThreadLocal() and !g->isDeclaration();
   }

   void emit_var (llvm::GlobalVariable *g, void *ptr) final
   {
      Varemit::emit_var (g, ptr);
      map[g] = (char*) ptr - (char*) dest;
   }
};

} // namespace

#endif
