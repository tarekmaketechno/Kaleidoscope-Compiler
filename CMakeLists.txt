cmake_minimum_required(VERSION 3.10)
set(CMAKE_CXX_STANDARD 11) 
set(CMAKE_C_STANDARD 11) 
project(CKC VERSION 0.1 
	DESCRIPTION "Kaleidoscope Compiler Project"
	LANGUAGES CXX C)

set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_C_COMPILER "clang")
set(CMAKE_LINKER "clang++")

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs support core irreader 
	executionengine mcjit passes interpreter analysis native bitwriter)

include_directories(src)
include_directories(inc)
include_directories(ext/hashtable)
include_directories (${CMAKE_BINARY_DIR})

find_package(BISON)
BISON_TARGET(Parser src/ekcc.y ${CMAKE_CURRENT_BINARY_DIR}/ekcc.tab.c 
	COMPILE_FLAGS -d DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/ekcc.tab.h)

find_package(FLEX)
FLEX_TARGET(Lexer src/ekcc.l ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.c)
ADD_FLEX_BISON_DEPENDENCY(Lexer Parser)

add_library(Printer inc/printer.h src/printer.c)
add_library(Hashtable ext/hashtable/hashtable.h ext/hashtable/hashtable.c)
add_library(Generator inc/generator.h src/generator.c)
add_library(Optimizer inc/optimizer.h src/optimizer.c)
add_library(Symbol inc/symbol.h src/symbol.c)
add_library(Bindings inc/bindings.h src/bindings.c)

add_library(Block src/block.cpp inc/block.hpp)


target_link_libraries(Symbol Hashtable)
target_link_libraries(Generator Bindings)

add_executable(ekcc 
	${BISON_Parser_OUTPUTS} ${FLEX_Lexer_OUTPUTS} 
	${CMAKE_CURRENT_BINARY_DIR}/ekcc.tab.h
	inc/ast.h src/ast.c src/ekcc.c)

target_link_libraries(ekcc Printer)
target_link_libraries(ekcc Symbol)
target_link_libraries(ekcc Generator)
target_link_libraries(ekcc Block)
target_link_libraries(ekcc -Wl,--whole-archive Bindings -Wl,--no-whole-archive, -rdynamic)
target_link_libraries(Block ${llvm_libs})
target_link_libraries(Optimizer Generator) 
target_link_libraries(ekcc Optimizer) 

target_link_libraries(ekcc ${llvm_libs})

