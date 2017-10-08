
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include <vector>
#include <memory>

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include "pta/fixpoint.hh"
#include "pta/node-base.hh"

#include "verbosity.h"

using namespace stid;

int main (int argc, char **argv)
{
   if (argc != 3)
   {
      std::cerr << "Usage: pta-dump <input file.{ll,bc}> <function name>\n";
      return 1;
   }

   // file to load and execute
   std::string path = argv[1];
   std::string function_name = argv[2];

   // related to the JIT engine, can we move it to some static class constructor
   // of the Executor??
   //llvm::InitializeNativeTarget();
   //llvm::InitializeNativeTargetAsmPrinter();
   //llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module *m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      err.print ("pta-dump", llvm::outs());
      return 1;
   }

   // find the function
   llvm::Function *f = m->getFunction (function_name);
   if (! f)
   {
      std::cerr << path << ": error: cannot find function '" << function_name
         << "'\n";
      return 1;
   }

   // run the analysis
   pta::Fixpoint fp (*f);
   pta::State &s = fp.run ();

   // print
   s.print (llvm::outs());
   return 1;
}
