#!/bin/sh

BUILD_DIR=${1:-build}

if [ ! -d $BUILD_DIR ]; then
    echo "The build directory does not exist.  Please run CMake and try again." >&2
    exit 1
fi

cmake --build $BUILD_DIR --config release

# Most of the time the "pyritec" executable will be in $BUILD_DIR/pyrite. However in Visual Studio it is placed in $BUILD_DIR/Release/pyritec.exe
PYRITEC=$BUILD_DIR/pyritec
if [ ! -f $PYRITEC ]; then
    PYRITEC=$BUILD_DIR/Release/pyritec.exe
    if [ ! -f $PYRITEC ]; then
        echo "The pyritec executable does not exist. This is a bug." >&2
        exit 1
    fi
fi

EXAMPLES_DIR=${2:-examples}

if [ ! -d $EXAMPLES_DIR ]; then
    echo "The examples directory does not exist. Please ensure it exists." >&2
    exit 1
fi

RT_C=${3:-src/rt.c}

if [ ! -f $RT_C ]; then
    echo "The runtime source file does not exist. Please ensure it exists." >&2
    exit 1
fi

CC=${CC:-clang}

for example in $EXAMPLES_DIR/*; do
    # There should be a file (main.pyrite) and possibly an output.txt file
    if [ ! -f $example/main.pyrite ]; then
        echo "The example $example does not have a main.pyrite file." >&2
        exit 1
    fi
    echo "Compiling $example"
    $PYRITEC $example/main.pyrite
    if [ $? -ne 0 ]; then
        echo "The example $example failed to compile." >&2
        exit 1
    fi
    echo "Linking $example"
    $CC -o $example/main $example/main.o $RT_C
    if [ $? -ne 0 ]; then
        echo "The example $example failed to compile with the runtime." >&2
        exit 1
    fi
    echo "Running $example"
    EXAMPLE_OUTPUT=`$example/main`
    if [ $? -ne 0 ]; then
        echo "The example $example failed to run." >&2
        echo "Output:" >&2
    echo "$EXAMPLE_OUTPUT" >&2
        exit 1
    fi
    if [ -f $example/output.txt ]; then
        EXPECTED_OUTPUT=`cat $example/output.txt`
        if [ "$EXAMPLE_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
            echo "The example $example did not produce the expected output." >&2
            echo "Expected: $EXPECTED_OUTPUT" >&2
            echo "Got: $EXAMPLE_OUTPUT" >&2
            exit 1
        fi
        else
            echo "Output:"
            echo "$EXAMPLE_OUTPUT"
    fi
    echo "Passed $example"
done
