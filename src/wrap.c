#include <string.h>

#include "wrap.h"

void parts_push(parts_t *parts, textpart_t part)
{
    if (parts->n == parts->cap) {
        size_t cap = parts->cap ? parts->cap * 2 : 8;
        textpart_t *v = xalloc(cap * sizeof *v);
        if (parts->v != nullptr)
            memcpy(v, parts->v, parts->n * sizeof *v);
        parts->v = v;
        parts->cap = cap;
    }
    parts->v[parts->n++] = part;
}

void lines_push(lines_t *lines, parts_t line)
{
    if (lines->n == lines->cap) {
        size_t cap = lines->cap ? lines->cap * 2 : 8;
        parts_t *v = xalloc(cap * sizeof *v);
        if (lines->v != nullptr)
            memcpy(v, lines->v, lines->n * sizeof *v);
        lines->v = v;
        lines->cap = cap;
    }
    lines->v[lines->n++] = line;
}

size_t part_len(textpart_t part)
{
    return utf8_len(part.s ? part.s : "");
}

size_t parts_len(const parts_t *parts)
{
    size_t len = 0;
    for (size_t i = 0; i < parts->n; i++)
        len += part_len(parts->v[i]) + (i > 0 ? 1 : 0);
    return len;
}

void show_words(sb_t *out, const parts_t *line)
{
    for (size_t i = 0; i < line->n; i++) {
        if (i > 0)
            sb_addc(out, ' ');
        textpart_t p = line->v[i];
        sb_add(out, p.opts);
        sb_add(out, p.s);
        sb_add(out, p.end);
    }
}

void show_lines(sb_t *out, const lines_t *lines)
{
    for (size_t i = 0; i < lines->n; i++) {
        if (i > 0)
            sb_addc(out, '\n');
        show_words(out, &lines->v[i]);
    }
}

/* Cut parts that are wider than the column into column-wide chunks, so that
 * the greedy pass below always finds room for a part on an empty line. */
static parts_t split_long_parts(size_t width, const parts_t *parts)
{
    parts_t out = {0};
    for (size_t i = 0; i < parts->n; i++) {
        textpart_t p = parts->v[i];
        if (part_len(p) <= width) {
            parts_push(&out, p);
            continue;
        }
        for (const char *rest = p.s; *rest != '\0'; rest = utf8_skip(rest, width))
            parts_push(&out, (textpart_t){
                                 .opts = p.opts,
                                 .s = utf8_take(rest, width),
                                 .end = p.end,
                             });
    }
    return out;
}

lines_t wrap(int width, const parts_t *parts)
{
    /* A column narrower than one character cannot make progress; the Haskell
     * loops forever there, we just fall back to a single character. */
    size_t n = width > 0 ? (size_t)width : 1;

    parts_t src = split_long_parts(n, parts);
    lines_t lines = {0};
    parts_t line = {0};
    size_t len = 0;

    for (size_t i = 0; i < src.n; i++) {
        size_t candidate = part_len(src.v[i]) + len + (len > 0 ? 1 : 0);
        if (candidate <= n) {
            parts_push(&line, src.v[i]);
            len = candidate;
        } else {
            lines_push(&lines, line);
            line = (parts_t){0};
            len = 0;
            i--; /* retry this part on the fresh line */
        }
    }
    lines_push(&lines, line); /* the last line, even when empty */
    return lines;
}

lines_t wrap_fill_start(int width, const parts_t *parts)
{
    size_t n = width > 0 ? (size_t)width : 1;
    lines_t lines = wrap(width, parts);

    for (size_t i = 0; i < lines.n; i++) {
        size_t len = parts_len(&lines.v[i]);
        if (len >= n)
            continue;

        /* One space of the padding is contributed by the joining space, so a
         * zero-length pad part is meaningful and must not be skipped. */
        size_t pad = n - len - 1;
        char *spaces = xalloc(pad + 1);
        memset(spaces, ' ', pad);

        parts_t padded = {0};
        parts_push(&padded, (textpart_t){ .s = spaces });
        for (size_t j = 0; j < lines.v[i].n; j++)
            parts_push(&padded, lines.v[i].v[j]);
        lines.v[i] = padded;
    }
    return lines;
}
