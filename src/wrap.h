/* Word wrapping over coloured text fragments.
 *
 * A textpart carries its ANSI prefix and reset suffix next to the text so that
 * wrapping can measure the visible text only.  Parts on a line are joined with
 * a single space, exactly like the Haskell `intersperse Space`. */
#ifndef LEU_WRAP_H
#define LEU_WRAP_H

#include <stddef.h>

#include "util.h"

typedef struct {
    const char *opts; /* concatenated SGR codes, innermost tag first */
    const char *s;    /* visible text */
    const char *end;  /* SGR reset */
} textpart_t;

typedef struct {
    textpart_t *v;
    size_t n;
    size_t cap;
} parts_t;

typedef struct {
    parts_t *v;
    size_t n;
    size_t cap;
} lines_t;

void parts_push(parts_t *parts, textpart_t part);
void lines_push(lines_t *lines, parts_t line);

size_t part_len(textpart_t part);       /* visible codepoints */
size_t parts_len(const parts_t *parts); /* including the joining spaces */

lines_t wrap(int width, const parts_t *parts);
lines_t wrap_fill_start(int width, const parts_t *parts); /* right aligned */

void show_words(sb_t *out, const parts_t *line);
void show_lines(sb_t *out, const lines_t *lines);

#endif
