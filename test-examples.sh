#!/bin/sh

BUILD_DIR=${BUILD_DIR:-build}

if [ ! -d $BUILD_DIR ]; then
    echo "The build directory does not exist.  Please run CMake and try again." >&2
    exit 1
fi

set -e

cmake --build $BUILD_DIR --config release

# We want to report our own errors.
set +e

# Most of the time the "pyritec" executable will be in $BUILD_DIR/pyrite. However in Visual Studio it is placed in $BUILD_DIR/Release/pyritec.exe
PYRITEC=$BUILD_DIR/pyritec
if [ ! -f $PYRITEC ]; then
    PYRITEC=$BUILD_DIR/Release/pyritec.exe
    if [ ! -f $PYRITEC ]; then
        echo "The pyritec executable does not exist. This is a bug." >&2
        exit 1
    fi
fi

EXAMPLES_DIR=${EXAMPLES_DIR:-examples}

if [ ! -d $EXAMPLES_DIR ]; then
    echo "The examples directory does not exist. Please ensure it exists." >&2
    exit 1
fi

RUNTIME_FILE=${RUNTIME_FILE:-src/rt.c}

if [ ! -f $RUNTIME_FILE ]; then
    echo "The runtime source file does not exist. Please ensure it exists." >&2
    exit 1
fi

CC=${CC:-clang}

EXAMPLES_TO_TEST=${@:-$(ls $EXAMPLES_DIR)}

for example in $EXAMPLES_TO_TEST; do
example_dir=$EXAMPLES_DIR/$example
    # There should be a file (main.pyrite) and possibly an output.txt file
    if [ ! -f $example_dir/main.pyrite ]; then
        echo "The example $example does not have a main.pyrite file." >&2
        exit 1
    fi
    echo "Compiling $example"
    $PYRITEC $example_dir/main.pyrite
    if [ $? -ne 0 ]; then
        echo "The example $example failed to compile." >&2
        exit 1
    fi
    echo "Linking $example"
    $CC -o $example_dir/main $example_dir/main.o $RUNTIME_FILE
    if [ $? -ne 0 ]; then
        echo "The example $example failed to link." >&2
        exit 1
    fi
    echo "Running $example"
    EXAMPLE_OUTPUT=`$example_dir/main`
    if [ $? -ne 0 ]; then
        echo "The example $example failed to run." >&2
        echo "Output:" >&2
    echo "$EXAMPLE_OUTPUT" >&2
        exit 1
    fi
    if [ -f $example_dir/output.txt ]; then
        EXPECTED_OUTPUT=`cat $example_dir/output.txt`
        if [ "$EXAMPLE_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
            echo "The example $example did not produce the expected output." >&2
            echo "Expected: $EXPECTED_OUTPUT" >&2
            echo "Got: $EXAMPLE_OUTPUT" >&2
            exit 1
        fi
        else
            # Save it for later (to verify that it doesn't change).
            echo "$EXAMPLE_OUTPUT" > $example_dir/output.txt
    fi
    echo "Passed $example"
done
