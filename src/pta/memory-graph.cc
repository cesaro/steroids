
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

MemoryNode *MemoryGraph::new_node (const llvm::Value *v)
{
   const llvm::Function *f;
   std::pair<Map::iterator,bool> ret;
   MemoryNode *n;

   // the value must be of pointer type
   ASSERT (v->getType()->isPointerTy());
   ASSERT (map.find(v) == map.end());

   // FIXME: link here to design notes, in order to justify why these
   // instruction types trigger the creation of these nodes

   if (llvm::isa<llvm::Function>(v))
   {
      nodes.emplace_back (MemoryNode::Type::Function, v);
      n = &nodes.back ();
   }
   else if (llvm::isa<llvm::GlobalVariable>(v))
   {
      nodes.emplace_back (MemoryNode::Type::GlobalVariable, v);
      n = &nodes.back ();
   }
   else if (llvm::isa<llvm::AllocaInst>(v))
   {
      // we create memory node of type Alloca, and initialize make it point to
      // *the* single node of type Inval we have in the graph, as when the llvm
      // instruction "alloca" allocates memory for a pointer, the initial value
      // is garbage
      nodes.emplace_back (MemoryNode::Type::Alloca, v);
      n = &nodes.back ();
      n->add (_invalid);
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
         nodes.emplace_back (MemoryNode::Type::Malloc, v);
         n = &nodes.back ();
      }
      else
      {
         // calls to other functions do not correspond to memory locations in
         // our abstraction
         return nullptr;
      }
   }
   else
   {
      // in our abstraction of the memory, instructions of this type do not
      // correspond to a memory block
      return nullptr;
   }

   // if we get here, then one of the if's above has created a node in the
   // Container nodes; we now insert a pointer to it in the map and return the
   // pointer
   ret = map.emplace (v, n);
   ASSERT (ret.second);
   return n;
}

/// This function is called only 3 times from the ctor of this class. It is not
/// called anywhere else
MemoryNode *MemoryGraph::new_node (MemoryNode::Type t)
{
   // construct a new MemoryNode in our container and return a pointer to it
   nodes.emplace_back (t);
   return &nodes.back ();
}

MemoryNode *MemoryGraph::operator[] (const llvm::Value *v)
{
   // if this Value is not pointer-typed, then we shall not have any memory node
   // associated to it
   if (!v->getType()->isPointerTy()) return nullptr;

   // if we have a node for this value in the map, find it and return it
   auto it = map.find (v);
   if (it != map.end()) return it->second;

   // otherwise examine the type of the value and if it is one for which the
   // analysis needs to associate a memory location (or node), then the
   // following function will find it, create a node in the Container and return
   // it; if the llvm value is not one of the expected nodes it will return null
   return new_node (v);
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
