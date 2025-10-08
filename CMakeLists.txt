cmake_minimum_required(VERSION 3.20)
project(wuyu VERSION 0.1.0 LANGUAGES C CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Find optional dependencies
find_package(ZLIB)
if(NOT ZLIB_FOUND)
    message(WARNING "ZLIB not found. Please install with: sudo apt install -y zlib1g-dev")
    message(WARNING "Attempting to build without it, but LLVM may require it...")
endif()

# Try to find readline for better REPL experience
find_library(READLINE_LIBRARY readline)
if(READLINE_LIBRARY)
    message(STATUS "Found readline: ${READLINE_LIBRARY}")
    set(HAVE_READLINE TRUE)
else()
    message(STATUS "readline not found - REPL will have basic line editing only")
    set(HAVE_READLINE FALSE)
endif()

# Find LLVM (version 20.x)
find_package(LLVM REQUIRED CONFIG HINTS /usr/lib/llvm-20/cmake/llvm /lib/llvm-20/cmake/llvm)

# Check LLVM version
if(LLVM_VERSION_MAJOR LESS 20)
    message(FATAL_ERROR "LLVM version must be at least 20, found ${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}")
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Add LLVM definitions and include directories
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Set compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")

# Utility library (static) for shared Chinese numeral parsing
add_library(wuyu_utils STATIC
    src/utils/numbers.cpp
)
# Set position independent code flag (needed for shared library linking)
set_target_properties(wuyu_utils PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Runtime library (shared) for Chinese numeral parsing
add_library(wuyu_runtime SHARED
    src/runtime.cpp
)

# Runtime depends on utils
target_link_libraries(wuyu_runtime wuyu_utils)

# Main executable
add_executable(wuyu
    src/main.cpp
)

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs
    core
    orcjit
    native
    support
    irreader
    passes
    target
    transformutils
    scalaropts
    instcombine
    aggressiveinstcombine
    analysis
    ipo
    mc
    mcparser
    asmparser
    asmprinter
    bitwriter
)

target_link_libraries(wuyu ${llvm_libs} wuyu_runtime wuyu_utils)
if(ZLIB_FOUND)
    target_link_libraries(wuyu ZLIB::ZLIB)
endif()
if(HAVE_READLINE)
    target_link_libraries(wuyu ${READLINE_LIBRARY})
    target_compile_definitions(wuyu PRIVATE HAVE_READLINE)
endif()

# Set output directory
set_target_properties(wuyu PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Install runtime library and executable
install(TARGETS wuyu_runtime DESTINATION lib)

# Install target
install(TARGETS wuyu DESTINATION bin)

# Print build information
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C++ flags: ${CMAKE_CXX_FLAGS}")

