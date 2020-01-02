#include "block.hpp"
#include "llvm/IR/BasicBlock.h"

extern "C" LLVMBasicBlockRef LLVMCreateBasicBlockInContext( LLVMContextRef C, const char *Name) {
	return wrap(llvm::BasicBlock::Create(*llvm::unwrap(C), Name));
}

