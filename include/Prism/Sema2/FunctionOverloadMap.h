#ifndef PRISM_SEMA2_FUNCTIONOVERLOADMAP_H
#define PRISM_SEMA2_FUNCTIONOVERLOADMAP_H

#include <string>

#include <utl/hashtable.hpp>

#include <Prism/Sema2/FuncSig.h>
#include <Prism/Sema2/SemaFwd.h>

namespace prism {

/// Maps function names and signatures to definitions. This is used to detect
/// function redefinitions
class FunctionOverloadMap {
public:
    struct Key {
        std::string name;
        GenericSignature gen_sig;
        FuncSig func_sig;
    };

    struct KeyView {
        std::string_view name;
        GenericSignature const& gen_sig;
        FuncSig const& func_sig;
    };

    FunctionDef* find(KeyView key) const {
        auto itr = _map.find(key);
        return itr != _map.end() ? itr->second : nullptr;
    }

    bool insert(Key key, FunctionDef* function) {
        return _map.try_emplace(std::move(key), function).second;
    }

private:
    struct Hash {
        using is_transparent = void;
        size_t operator()(auto const& key) const {
            return utl::hash_combine(key.name, key.gen_sig,
                                     key.func_sig
                                         .hash_value_ignoring_return_type());
        }
    };
    struct Compare {
        using is_transparent = void;
        bool operator()(auto const& a, auto const& b) const {
            return a.name == b.name && a.gen_sig == b.gen_sig &&
                   a.func_sig.compare_ignoring_return_type(b.func_sig);
        }
    };

    utl::hashmap<Key, FunctionDef*, Hash, Compare> _map;
};

} // namespace prism

#endif // PRISM_SEMA2_FUNCTIONOVERLOADMAP_H
