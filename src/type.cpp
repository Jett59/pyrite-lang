#include "type.h"

namespace pyrite {
    std::unique_ptr<Type> cloneType(const Type &type) {
        // The PartialTypeToTypeTransformVisitor copies all elements by default, which means it is perfectly suited to this task.
        PartialTypeToTypeTransformVisitor transformer;
        return transformer.visit(type);
    }
}
