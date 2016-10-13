
#ifndef __MISC_HH_
#define __MISC_HH_

#include <string>

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

std::string fmt (const std::string fmt_str, ...);
std::string operator * (std::string lhs, int i);

std::string quoted_str (const char * str);

void print_type (llvm::Type *t, std::string &s);
void print_value (llvm::Value *v, std::string &s);
void print_value_as_operand (llvm::Value *v, std::string &s);

#endif
