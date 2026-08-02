/* Renders one part into the string that is printed. */
#ifndef LEU_RENDER_H
#define LEU_RENDER_H

#include "model.h"
#include "util.h"

const char *render_part(terminal_t term, const part_t *part);

/* Exposed for the tests: the SGR prefix leo's inline markup maps to. */
const char *tag_to_sgr(const char *tag, bool color);

#endif
