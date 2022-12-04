#ifndef PYRITE_HELPER_H
#define PYRITE_HELPER_H

#include <memory>

namespace pyrite {
    template<typename Derived, typename Base>
    static inline std::unique_ptr<Derived> staticCast(std::unique_ptr<Base> &&base) {
        return std::unique_ptr<Derived>(static_cast<Derived *>(base.release()));
    }
}

#endif