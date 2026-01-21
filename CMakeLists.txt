cmake_minimum_required(VERSION 3.16)
project(banglascript)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Force static library lookup
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".lib")
set(BUILD_SHARED_LIBS OFF)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Source files
set(SOURCES
    src/main.cpp
)

add_executable(banglascript ${SOURCES})

# Get all required LLVM libraries
llvm_map_components_to_libnames(llvm_libs
    Core
    Support
    IRReader
    ExecutionEngine
    MCJIT
    OrcJIT
    Interpreter
    RuntimeDyld
    X86CodeGen
    X86AsmParser
    X86Desc
    X86Info
    Target
    MC
    MCParser
    MCDisassembler
    Object
    BitReader
    BitWriter
    Passes
    Analysis
    TransformUtils
    ScalarOpts
    InstCombine
    AggressiveInstCombine
    CodeGen
    SelectionDAG
    AsmPrinter
    GlobalISel
    CFGuard
    Demangle
    BinaryFormat
    Remarks
    BitstreamReader
    TargetParser
    native
    nativecodegen
)

# Platform-specific settings
if(WIN32)
    # Windows static linking
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static -static-libgcc -static-libstdc++")
    
    target_link_libraries(banglascript
        ${llvm_libs}
        version
        ole32
        uuid
        ws2_32
        psapi
        shell32
        advapi32
        ntdll
    )
else()
    # Linux static linking
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgcc -static-libstdc++")
    
    target_link_libraries(banglascript
        ${llvm_libs}
        pthread
        dl
        m
    )
endif()

# Set properties for better static linking
set_target_properties(banglascript PROPERTIES
    LINK_SEARCH_START_STATIC ON
    LINK_SEARCH_END_STATIC ON
)
