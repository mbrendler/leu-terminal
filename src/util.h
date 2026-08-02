/* Small helpers: allocation, string buffers, UTF-8, terminal detection. */
#ifndef LEU_UTIL_H
#define LEU_UTIL_H

#include <stddef.h>

/* leu is a one-shot process: it allocates, prints and exits.  Nothing is ever
 * freed on purpose - that keeps every ownership question out of the code. */
void *xalloc(size_t size);
char *xstrdup(const char *s);

/* Growable string buffer.  Zero-initialise to create one: sb_t sb = {0}; */
typedef struct {
    char *s;
    size_t len;
    size_t cap;
} sb_t;

void sb_add(sb_t *sb, const char *s);
void sb_addn(sb_t *sb, const char *s, size_t n);
void sb_addc(sb_t *sb, char c);
void sb_addf(sb_t *sb, const char *fmt, ...);
const char *sb_str(const sb_t *sb); /* never NULL */

/* UTF-8.  The Haskell version measured Data.Char values, so lengths are
 * counted in codepoints, not bytes. */
size_t utf8_len(const char *s);
char *utf8_take(const char *s, size_t n);  /* first n codepoints, copied */
const char *utf8_skip(const char *s, size_t n);

char *str_lower(const char *s);

typedef struct {
    int width;
    bool color;
} terminal_t;

terminal_t term_detect(void);

#endif
