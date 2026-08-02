#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "util.h"

void *xalloc(size_t size)
{
    void *p = calloc(1, size ? size : 1);
    if (p == nullptr) {
        fputs("leu: out of memory\n", stderr);
        exit(1);
    }
    return p;
}

char *xstrdup(const char *s)
{
    if (s == nullptr)
        s = "";
    size_t n = strlen(s) + 1;
    char *copy = xalloc(n);
    memcpy(copy, s, n);
    return copy;
}

static void sb_reserve(sb_t *sb, size_t extra)
{
    if (sb->len + extra + 1 <= sb->cap)
        return;
    size_t cap = sb->cap ? sb->cap : 64;
    while (cap < sb->len + extra + 1)
        cap *= 2;
    char *s = xalloc(cap);
    if (sb->s != nullptr)
        memcpy(s, sb->s, sb->len);
    sb->s = s;
    sb->cap = cap;
}

void sb_addn(sb_t *sb, const char *s, size_t n)
{
    if (s == nullptr || n == 0)
        return;
    sb_reserve(sb, n);
    memcpy(sb->s + sb->len, s, n);
    sb->len += n;
    sb->s[sb->len] = '\0';
}

void sb_add(sb_t *sb, const char *s)
{
    if (s != nullptr)
        sb_addn(sb, s, strlen(s));
}

void sb_addc(sb_t *sb, char c)
{
    sb_addn(sb, &c, 1);
}

void sb_addf(sb_t *sb, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    va_list ap2;
    va_copy(ap2, ap);
    int n = vsnprintf(nullptr, 0, fmt, ap);
    va_end(ap);
    if (n > 0) {
        sb_reserve(sb, (size_t)n);
        vsnprintf(sb->s + sb->len, (size_t)n + 1, fmt, ap2);
        sb->len += (size_t)n;
    }
    va_end(ap2);
}

const char *sb_str(const sb_t *sb)
{
    return sb->s ? sb->s : "";
}

/* Continuation bytes (0b10xxxxxx) do not start a codepoint. */
static bool is_cont(unsigned char c)
{
    return (c & 0xc0) == 0x80;
}

size_t utf8_len(const char *s)
{
    size_t n = 0;
    for (const unsigned char *p = (const unsigned char *)s; *p != '\0'; p++)
        if (!is_cont(*p))
            n++;
    return n;
}

const char *utf8_skip(const char *s, size_t n)
{
    const unsigned char *p = (const unsigned char *)s;
    while (n > 0 && *p != '\0') {
        p++;
        while (is_cont(*p))
            p++;
        n--;
    }
    return (const char *)p;
}

char *utf8_take(const char *s, size_t n)
{
    const char *end = utf8_skip(s, n);
    size_t bytes = (size_t)(end - s);
    char *copy = xalloc(bytes + 1);
    memcpy(copy, s, bytes);
    return copy;
}

char *str_lower(const char *s)
{
    char *copy = xstrdup(s);
    for (char *p = copy; *p != '\0'; p++)
        *p = (char)tolower((unsigned char)*p);
    return copy;
}

/* Mirrors buildTerminal: terminal-size with an 80x25 fallback, plus
 * ansi-terminal's hSupportsANSI (a tty whose TERM is set and not "dumb"). */
terminal_t term_detect(void)
{
    constexpr int default_width = 80;

    int width = default_width;
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0)
        width = ws.ws_col;

    const char *term = getenv("TERM");
    bool color = isatty(STDOUT_FILENO) && term != nullptr && *term != '\0'
                 && strcmp(term, "dumb") != 0;

    return (terminal_t){ .width = width, .color = color };
}
