
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/Function.h"

#include "pta/state.hh"

namespace stid {
namespace pta {

void State::print(llvm::raw_ostream &os) const
{
   os << "== begin pta analysis result ==\n";
   // print the memory graph
   memory.print (os);
   // print the global variables found during the analysis
   for (auto &kv : valuation.map)
   {
      if (auto g = llvm::dyn_cast<const llvm::GlobalVariable>(kv.first))
      {
         print (os, *g);
      }
   }
   os << "\n";

   // print the llvm bitcode of all functions found during the analysis,
   // annotated with the objects pointed
   os << "== begin annotated listing ==\n";
   for (auto &kv : valuation.map)
   {
      if (auto f = llvm::dyn_cast<const llvm::Function>(kv.first))
      {
         print (os, *f);
         os << "\n";
      }
   }
   os << "== end annotated listing ==\n";
   os << "== end pta analysis result ==\n";
}

void State::print(llvm::raw_ostream &os, const llvm::GlobalVariable &g) const
{
   os << "@" << g.getName() << " = ";
   if (g.isDeclaration()) os << "external ";
   if (g.isThreadLocal()) os << "thread_local ";
   os << (g.isConstant() ? "constant " : "global ");
   g.getType()->print (os);
   if (! g.isDeclaration())
   {
      os << " " << g.getInitializer();
   }
   os << "\n";
}

void State::print(llvm::raw_ostream &os, const llvm::Function &f) const
{
   int count;

   os << "; FIXME: arguments\n";

   // print header
   os << (f.isDeclaration() ?  "declare " : "define ");
   f.getReturnType()->print (os);
   os << " @" << f.getName() << " (";
   count = f.arg_size();
   for (const auto &arg : f.args())
   {
      arg.getType()->print (os);
      os << " " << arg.getName();
      if (count >= 2) os << ", ";
      --count;
   }
   if (f.isVarArg()) os << ", ...";
   os << ")\n";
   if (f.isDeclaration()) return;

   // print body
   os << "{";
   for (const llvm::BasicBlock &b : f)
   {
      os << "\n";
      print (os, b);
   }
   os << "}\n";
}

void State::print(llvm::raw_ostream &os, const llvm::BasicBlock &b) const
{
   // print label
   b.printAsOperand(os);
   os << ":\n";

   // print body
   for (const llvm::Instruction &in : b)
   {
      print (os, in);
   }
}

void State::print(llvm::raw_ostream &os, const llvm::Instruction &in) const
{
   int opn, i;
   const PointerValue *val;
   const llvm::Value *v;

   // for every pointer operand, print the pointed memory locations
   opn = in.getNumOperands();
   for (i = 0; i < opn; i++)
   {
      v = in.getOperand(i);
      if (! v->getType()->isPointerTy()) continue;
      val = valuation.get(v);
      if (val)
         val->print (os, "  ; ");
      else
      {
         os << "; Pointer ";
         v->printAsOperand (os);
         os << ": no info\n";
      }
   }

   // print the instruction
   os << in << "\n";
}

void State::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
