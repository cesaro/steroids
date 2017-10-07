
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "pta/memory-graph.hh"

#include "verbosity.h"

namespace stid {
namespace pta {

// private to this file
struct NameType
{
   std::string name;
   std::string type;

   //bool operator== (const NameType &other)
   //{
   //   return name == other.name and type == other.type;
   //}
};

bool operator< (const NameType &a, const NameType &b)
{
   return a.name < b.name and a.type < b.type;
}

// list of functions (names and types) that can allocate new memory
std::set<NameType> _malloc_funs = {
   {"malloc", "void*(i64)"}
};

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const MemoryGraph &g)
{
   g.print (os);
   return os;
}

bool MemoryGraph::find_type (const llvm::Value *v, MemoryNode::Type &t)
{
   const llvm::Function *f;

   // the value must be of pointer type
   ASSERT (v->getType()->isPointerTy());
   ASSERT (map.find(v) == map.end());

   // FIXME: link here to design notes

   if (llvm::isa<llvm::Function>(v))
   {
      t = MemoryNode::Type::Function;
      return true;
   }
   else if (llvm::isa<llvm::GlobalVariable>(v))
   {
      t = MemoryNode::Type::GlobalVariable;
      return true;
   }
   else if (llvm::isa<llvm::AllocaInst>(v))
   {
      t = MemoryNode::Type::Alloca;
      return true;
   }
   else if (auto c = llvm::dyn_cast<llvm::CallInst>(v))
   {
      std::string type;
      llvm::raw_string_ostream os (type);

      f = c->getCalledFunction();
      f->getType()->print (os);
      if (_malloc_funs.find ({f->getName().str(), os.str()}) !=
         _malloc_funs.end())
      {
         t = MemoryNode::Type::Malloc;
         return true;
      }
   }

   return false;
}

MemoryNode *MemoryGraph::new_node (MemoryNode::Type t, const llvm::Value *v)
{
   std::pair<Map::iterator,bool> ret;
   MemoryNode *n;

   // construct and insert a new MemoryNode in our container, insert a pointer
   // to it in the map and return the pointer
   nodes.emplace_back (t, v);
   n = &nodes.back ();
   ret = map.emplace (v, n);
   ASSERT (ret.second);
   return n;
}

MemoryNode *MemoryGraph::new_node (MemoryNode::Type t)
{
   // construct a new MemoryNode in our container and return a pointer to it
   nodes.emplace_back (t);
   return &nodes.back ();
}

MemoryNode *MemoryGraph::operator[] (const llvm::Value *v)
{
   MemoryNode::Type t;
   bool found;

   // if this Value is not pointer-typed, then we shall not have any memory node
   // associated to it
   if (!v->getType()->isPointerTy()) return nullptr;

   // if we have a node for this value in the map, find it and return it
   auto it = map.find (v);
   if (it != map.end()) return it->second;

   // otherwise examine the type of the value and if it is one for which the
   // analysis needs to associate a memory location (or node), then the
   // following function will find it and return true
   found = find_type (v, t);
   if (! found) return nullptr;
   
   // we create MemoryNode, insert it in the map and return its address
   return new_node (t, v);
}

void MemoryGraph::print (llvm::raw_ostream &os) const
{
   os << "== begin memory graph ==\n";
   os << "* " << nodes.size() << " nodes, ";
   os << map.size() << " mapped values\n";
   for (const MemoryNode &n : nodes) os << n;
   os << "== begin memory graph ==\n";
}

void MemoryGraph::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
