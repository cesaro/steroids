
#ifndef __STID_PTA_MEMORYGRAPH_HH_
#define __STID_PTA_MEMORYGRAPH_HH_

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <map>
#include <list>

#include "pta/memory-node.hh"

namespace stid {
namespace pta {

class MemoryGraph
{
private:
   typedef std::map<const llvm::Value*, MemoryNode*> Map;
   typedef std::list<MemoryNode> Container;

   /// A map pointing llvm values of selected types (e.g, alloca instructions,
   /// global variables, functions, see find_type()) to the memory nodes they
   /// gave rise to. The pointers point to objects stored in the memory
   /// container, field  #nodes.
   Map map;

   /// A list storing all MemoryNode's in this graph.
   Container nodes;

   // Pointers to memory nodes of specific kind. There is exactly 1 node of each
   // kind for the following:
   MemoryNode *_top;
   MemoryNode *_nullptr;
   MemoryNode *_invalid;

   bool find_type (const llvm::Value *v, MemoryNode::Type &t);
   MemoryNode *new_node (const llvm::Value *v);
   MemoryNode *new_node (MemoryNode::Type t);

public:
   MemoryGraph () :
      map (),
      nodes (),
      _top (new_node (MemoryNode::Type::Top)),
      _nullptr (new_node (MemoryNode::Type::Nullptr)),
      _invalid (new_node (MemoryNode::Type::Invalid))
   {}

   typedef Container::iterator iterator;
   typedef Container::const_iterator const_iterator;

   iterator begin () { return nodes.begin(); }
   iterator end () { return nodes.end(); }

   const_iterator begin () const { return nodes.begin(); }
   const_iterator end () const { return nodes.end(); }

   /// Gets the memory node of type Function, GlobalVariable, Alloca or Malloc
   /// associated to a LLVM function, global variable, alloca instruction or
   /// call to malloc()
   MemoryNode *operator[] (const llvm::Value *);

   // Return the unique memory node of type Top, Invalid, or Nullptr
   MemoryNode *top () { return _top; }
   MemoryNode *nullptr_ () { return _nullptr; }
   MemoryNode *invalid () { return _invalid; }

   void print (llvm::raw_ostream &os) const;
   void dump () const;
};

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const MemoryGraph &g);

#if 0
   /// An iterator through the list of nodes in the map.
   typedef<IterType, RetType>
   class It
   {
   public:
      bool operator== (const It &other) const { return it == other.it; }
      bool operator!= (const It &other) const { return it != other.it; }
      It &operator++ () { ++it; return *this; }
      It operator++ (int) { It<IterType,RetType> r (*this); ++it; return r; }
      RetType operator* () { return it->second; }
   private:
      It (IterType *it_) : it (it_) {};
      IterType it;
      friend class MemoryGraph;
   };

   typedef It<NodeMap::iterator, MemoryNode&> iterator
   typedef It<NodeMap::const_iterator, const MemoryNode&> const_iterator

   iterator begin () { return iterator (map.begin()); }
   iterator end () { return iterator (map.end()); }

   const_iterator begin () const { return const_iterator (map.begin()); }
   const_iterator end () const { return const_iterator (map.end()); }
#endif

} // pta
} // stid

#endif
