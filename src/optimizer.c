#include "optimizer.h"

void set_optimizations(LLVMPassManagerRef function_passes,
		  LLVMPassManagerRef module_passes, int optimize_level){

	 switch(optimize_level) {
		  case 0:
			   printf("Applying No Optimizations!\n");
			   break;
		  case 1:
			   break;
		  case 2:
			   LLVMAddBasicAliasAnalysisPass(function_passes);
			   LLVMAddBasicAliasAnalysisPass(module_passes);
			   LLVMAddTypeBasedAliasAnalysisPass(function_passes);
			   LLVMAddScopedNoAliasAAPass(function_passes);
			   LLVMAddAggressiveDCEPass(function_passes);
			   LLVMAddBitTrackingDCEPass(function_passes);
			   LLVMAddAggressiveInstCombinerPass(function_passes);
			   break;
		  case 3:
			   LLVMAddLoopDeletionPass(function_passes);
			   LLVMAddLoopIdiomPass(function_passes);
			   LLVMAddLoopRotatePass(function_passes);
			   LLVMAddLoopRerollPass(function_passes);
			   LLVMAddLoopUnrollPass(function_passes);
			   LLVMAddLoopUnrollAndJamPass(function_passes);
			   LLVMAddLoopUnswitchPass(function_passes);
			   break;
		  case 4:
			   LLVMAddAlignmentFromAssumptionsPass(function_passes);
			   LLVMAddCFGSimplificationPass(function_passes);
			   LLVMAddDeadStoreEliminationPass(function_passes);
			   LLVMAddScalarizerPass(function_passes);
			   LLVMAddMergedLoadStoreMotionPass(function_passes);
			   break;
		  case 5:
			   LLVMAddGVNPass(function_passes);
			   LLVMAddNewGVNPass(function_passes);
			   LLVMAddIndVarSimplifyPass(function_passes);
			   LLVMAddInstructionCombiningPass(function_passes);
			   LLVMAddJumpThreadingPass(function_passes);
			   break;
		  case 6:
			   LLVMAddLICMPass(function_passes);
			   LLVMAddMemCpyOptPass(function_passes);
			   LLVMAddPartiallyInlineLibCallsPass(function_passes);
			   LLVMAddReassociatePass(function_passes);
			   LLVMAddSCCPPass(function_passes);
			   break;
		  case 7:
			   LLVMAddScalarReplAggregatesPass(function_passes);
			   LLVMAddScalarReplAggregatesPassSSA(function_passes);
			   LLVMAddScalarReplAggregatesPassWithThreshold(function_passes, 10);
			   LLVMAddSimplifyLibCallsPass(function_passes);
			   break;
		  case 8:
			   LLVMAddConstantPropagationPass(function_passes);
			   LLVMAddDemoteMemoryToRegisterPass(function_passes);
			   LLVMAddVerifierPass(function_passes);
			   LLVMAddCorrelatedValuePropagationPass(function_passes);
			   break;
		  case 9:
			   LLVMAddEarlyCSEPass(function_passes);
			   LLVMAddEarlyCSEMemSSAPass(function_passes);
			   LLVMAddLowerExpectIntrinsicPass(function_passes);
			   break;
		  case 10:
			   LLVMAddLoopVectorizePass(function_passes);
			   LLVMAddSLPVectorizePass(function_passes);
			   break;
		  case 11:
			   LLVMAddGlobalDCEPass(module_passes);
			   LLVMAddGlobalOptimizerPass(module_passes);
			   LLVMAddIPConstantPropagationPass(module_passes);
			   LLVMAddPruneEHPass(module_passes);
			   LLVMAddIPSCCPPass(module_passes);
			   break;
		  case 12:
			   LLVMAddArgumentPromotionPass(module_passes);
			   LLVMAddConstantMergePass(module_passes);
			   // LLVMAddInternalizePass(module_passes, 10);
			   LLVMAddStripDeadPrototypesPass(module_passes);
			   LLVMAddStripSymbolsPass(module_passes);
			   break;
		  case 13:
			   LLVMAddCalledValuePropagationPass(module_passes);
			   LLVMAddDeadArgEliminationPass(module_passes);
			   // LLVMAddFunctionAttrsPass(module_passes);
			   LLVMAddFunctionInliningPass(module_passes);
			   LLVMAddAlwaysInlinerPass(module_passes);
			   break;
	 }

}

int optimize(generator_t * gen, int optimize_level) {

	LLVMPassManagerBuilderRef passBuilder;

	passBuilder = LLVMPassManagerBuilderCreate();

	LLVMPassManagerBuilderSetOptLevel(passBuilder, 3);
	LLVMPassManagerBuilderSetSizeLevel(passBuilder, 0);

	LLVMPassManagerRef functionPasses =
		 LLVMCreateFunctionPassManagerForModule(
				   gen->module);

	LLVMPassManagerRef modulePasses =
		 LLVMCreatePassManager();

	if(optimize_level == 1) {
		LLVMPassManagerBuilderPopulateFunctionPassManager(
				passBuilder, functionPasses);

		LLVMPassManagerBuilderPopulateModulePassManager(
				passBuilder, modulePasses);

	} else {
		 set_optimizations(functionPasses, 
				   modulePasses, optimize_level);

	}

	LLVMPassManagerBuilderDispose(passBuilder);

	LLVMInitializeFunctionPassManager(functionPasses);

	for (LLVMValueRef value = LLVMGetFirstFunction(gen->module);
			  value; value = LLVMGetNextFunction(value)) {

		 LLVMRunFunctionPassManager(functionPasses, value);

	}

	LLVMFinalizeFunctionPassManager(functionPasses);

	LLVMRunPassManager(modulePasses, gen->module);

	LLVMDisposePassManager(functionPasses);
	LLVMDisposePassManager(modulePasses);
	return 0;
}


