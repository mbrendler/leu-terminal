#include <string.h>

#include "model.h"
#include "util.h"

/* The order is the order of the Haskell `LanguageMapping` enum, which is also
 * the order --show-languages prints and whose first entry is the default. */
const lang_t languages[] = {
    { .code = "ende", .pretty = "EnDe: English    - German" },
    { .code = "frde", .pretty = "FrDe: French     - German" },
    { .code = "esde", .pretty = "EsDe: Spanish    - German" },
    { .code = "itde", .pretty = "ItDe: Italian    - German" },
    { .code = "chde", .pretty = "ChDe: Chinese    - German" },
    { .code = "rude", .pretty = "RuDe: Russian    - German" },
    { .code = "ptde", .pretty = "PtDe: Portuguese - German" },
    { .code = "plde", .pretty = "PlDe: Polish     - German" },
};

const size_t nlanguages = sizeof languages / sizeof languages[0];

const lang_t *lang_lookup(const char *name)
{
    if (name != nullptr) {
        const char *wanted = str_lower(name);
        for (size_t i = 0; i < nlanguages; i++)
            if (strcmp(wanted, languages[i].code) == 0)
                return &languages[i];
    }
    return &languages[0];
}
