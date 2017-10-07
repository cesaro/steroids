
#ifndef __MISC_HH_
#define __MISC_HH_

#include <string>

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Module.h>

#define UNITS_UNIT(s) \
      (s) < 2048 ? "B" : \
         (s) / 1024 < 1024 ? "K" : \
            (s) / (1024 * 1024) < 1024 ? "M" : "G"

#define UNITS_SIZE(s) \
      (s) < 2048 ? (s) : \
         (s) / 1024 < 1024 ? (s) / 1024 : \
            (s) / (1024 * 1024) < 1024 ? (s) / (1024 * 1024) : \
               (s) / (size_t) (1024 * 1024 * 1024)

std::string fmt (const std::string fmt_str, ...);
std::string operator * (std::string lhs, int i);

std::string quoted_str (const char * str);

void print_type (llvm::Type *t, std::string &s);
void print_value (const llvm::Value *v, std::string &s);
void print_value_as_operand (llvm::Value *v, std::string &s);
void dump_ll (const llvm::Module *m, const char *filename);

std::string str (const llvm::Value *v);

#endif
