/* leu - a command line tool to query dict.leo.org. */
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libxml/parser.h>

#include "http.h"
#include "parse.h"
#include "render.h"
#include "util.h"
#include "wrap.h"

/* The wording parseargs produced, kept verbatim - typo included. */
static void usage(const char *program, FILE *to)
{
    fprintf(to, "usage: %s [options] [--] [SEARCH ...]\n", program);
    fprintf(to, "  [-f,--file <FILE>]        work with FILE instead of a "
                "HTTP-Request\n");
    fprintf(to, "  [-x,--show-xml-response]  show XML response instead of parsed "
                "Translations\n");
    fprintf(to, "  [-l,--language <LANG>]    use another language default is ende\n");
    fprintf(to, "  [--show-languages]        show all supported langages\n");
    fprintf(to, "\n");
}

static char *read_file(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (f == nullptr) {
        fprintf(stderr, "leu: %s: cannot open file\n", path);
        exit(1);
    }

    sb_t body = {0};
    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof buf, f)) > 0)
        sb_addn(&body, buf, n);
    if (ferror(f)) {
        fprintf(stderr, "leu: %s: read error\n", path);
        exit(1);
    }
    fclose(f);

    return body.s ? body.s : xstrdup("");
}

static char *join_args(int argc, char **argv, int from)
{
    sb_t s = {0};
    for (int i = from; i < argc; i++) {
        if (i > from)
            sb_addc(&s, ' ');
        sb_add(&s, argv[i]);
    }
    return s.s ? s.s : xstrdup("");
}

/* getProgName reported the plain name, not the path it was started with. */
static const char *program_name(const char *argv0)
{
    const char *slash = strrchr(argv0, '/');
    return slash != nullptr ? slash + 1 : argv0;
}

int main(int argc, char **argv)
{
    LIBXML_TEST_VERSION

    enum { OPT_SHOW_LANGUAGES = 1000 };
    static const struct option long_options[] = {
        { .name = "file",              .has_arg = required_argument, .val = 'f' },
        { .name = "show-xml-response", .has_arg = no_argument,       .val = 'x' },
        { .name = "language",          .has_arg = required_argument, .val = 'l' },
        { .name = "show-languages",    .has_arg = no_argument, .val = OPT_SHOW_LANGUAGES },
        { .name = "help",              .has_arg = no_argument,       .val = 'h' },
        {0},
    };

    const char *file = nullptr;
    const char *language = nullptr;
    bool show_xml = false;

    for (int c; (c = getopt_long(argc, argv, "f:xl:h", long_options, nullptr)) != -1;) {
        switch (c) {
        case 'f': file = optarg; break;
        case 'x': show_xml = true; break;
        case 'l': language = optarg; break;
        case OPT_SHOW_LANGUAGES:
            for (size_t i = 0; i < nlanguages; i++)
                puts(languages[i].pretty);
            putchar('\n'); /* the list has always ended with a blank line */
            return 0;
        /* parseargs had no help flag and answered -h with an error; a working
         * one is the only place this deviates from the Haskell wording. */
        case 'h': usage(program_name(argv[0]), stdout); return 0;
        default: usage(program_name(argv[0]), stderr); return 1;
        }
    }

    const lang_t *lang = lang_lookup(language);
    printf("use language: %s\n", lang->pretty);

    char *response = file != nullptr ? read_file(file)
                                     : http_query(join_args(argc, argv, optind), lang);

    if (show_xml) {
        fputs(response, stdout);
        fputc('\n', stdout);
        return 0;
    }

    terminal_t term = term_detect();
    partlist_t parts = parse_document(response);

    if (parts.n == 0) {
        puts("No translation found.");
        puts("Use '-x' to show the XML response.");
        return 0;
    }

    /* leo sends the least interesting block first, so parts print backwards. */
    for (size_t i = parts.n; i > 0; i--) {
        fputs(render_part(term, &parts.v[i - 1]), stdout);
        fputc('\n', stdout);
    }
    return 0;
}
