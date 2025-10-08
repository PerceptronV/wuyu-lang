#!/bin/bash

# Build script for wuyu compiler
# Usage: ./build.sh [clean|release|debug]

set -e

BUILD_TYPE="Release"
BUILD_DIR="build"

# Parse arguments
if [ "$1" = "clean" ]; then
    echo "Cleaning build directories..."
    rm -rf build build-debug build-release
    exit 0
elif [ "$1" = "debug" ]; then
    BUILD_TYPE="Debug"
    BUILD_DIR="build-debug"
elif [ "$1" = "release" ]; then
    BUILD_TYPE="Release"
    BUILD_DIR="build-release"
fi

echo "Building wuyu compiler (${BUILD_TYPE})..."

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Check if Ninja is available
if command -v ninja &> /dev/null; then
    GENERATOR="-G Ninja"
    BUILD_CMD="ninja"
else
    GENERATOR=""
    BUILD_CMD="make -j$(nproc)"
fi

# Configure with CMake
echo "Configuring with CMake..."
cmake -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
      $GENERATOR \
      ..

# Build
echo "Building Wuyu..."
$BUILD_CMD

echo ""
echo "✓ Build complete!"
echo "Executable: ${BUILD_DIR}/bin/wuyu"
echo ""
echo "To test: ./${BUILD_DIR}/bin/wuyu --version"
echo "Run example: ./${BUILD_DIR}/bin/wuyu docs/examples/質數.🐲"
