SET(CMAKE_BUILD_TYPE Debug)
project(cpi)
cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_FLAGS "-D_DEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -std=c++11 -fvisibility-inlines-hidden -fno-exceptions -fno-rtti -fno-common -Woverloaded-virtual -Wcast-qual")
aux_source_directory(. SRC_LIST)
aux_source_directory(./visitors SRC_LIST)
include_directories(/usr/local/Cellar/llvm/3.5.0_2/include/ ./json/src/)
link_directories(/usr/local/lib /usr/local/Cellar/llvm/3.5.0_2/lib)

add_executable(${PROJECT_NAME} ${SRC_LIST})
target_link_libraries(cpi cppformat -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMX86Desc -lLLVMObject -lLLVMMCParser -lLLVMBitReader -lLLVMX86Info -lLLVMX86AsmPrinter -lLLVMX86Utils -lLLVMInterpreter -lLLVMJIT -lLLVMCodeGen -lLLVMScalarOpts -lLLVMInstCombine -lLLVMTransformUtils -lLLVMipa -lLLVMExecutionEngine -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMCore -lLLVMBitWriter -lLLVMSupport -lz -lpthread -ledit -lcurses -lm)
add_definitions(-std=c++11 -fexceptions)
