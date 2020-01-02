#pragma once

#include "llvm-c/Core.h"

#ifdef __cplusplus
extern "C"
#endif
LLVMBasicBlockRef LLVMCreateBasicBlockInContext( LLVMContextRef C, const char *Name);


#ifdef __cplusplus
extern "C"
#endif
LLVMValueRef LLVMConstInt(LLVMTypeRef IntTy, unsigned long long N, LLVMBool SignExtend);


