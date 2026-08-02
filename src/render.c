#include <string.h>

#include "parse.h"
#include "render.h"
#include "wrap.h"

/* The codes ansi-terminal produced: dull colours are 3x, vivid ones 9x, and
 * setSGRCode [] is a bare "ESC [ m" rather than "ESC [ 0 m". */
#define SGR_RESET "\033[m"
#define SGR_DULL_WHITE "\033[37m"
#define SGR_DULL_YELLOW "\033[33m"
#define SGR_DULL_RED "\033[31m"
#define SGR_VIVID_BLUE "\033[94m"

static const struct {
    const char *tag;
    const char *sgr;
} tag_colors[] = {
    { .tag = "b",           .sgr = SGR_VIVID_BLUE  },
    { .tag = "small",       .sgr = SGR_DULL_YELLOW },
    { .tag = "sup",         .sgr = SGR_DULL_WHITE  },
    { .tag = "domain",      .sgr = SGR_DULL_WHITE  },
    { .tag = "flecttabref", .sgr = SGR_DULL_WHITE  },
    { .tag = "i",           .sgr = ""              },
    { .tag = "repr",        .sgr = ""              },
    { .tag = "br",          .sgr = ""              },
    { .tag = "t",           .sgr = ""              },
    { .tag = "m",           .sgr = ""              },
    { .tag = "sr",          .sgr = ""              },
};

const char *tag_to_sgr(const char *tag, bool color)
{
    if (!color)
        return "";
    for (size_t i = 0; i < sizeof tag_colors / sizeof tag_colors[0]; i++)
        if (strcmp(tag, tag_colors[i].tag) == 0)
            return tag_colors[i].sgr;

    sb_t sb = {0};
    sb_addf(&sb, "UNHANDLED TAGNAME (%s)", tag);
    return sb_str(&sb);
}

/* The SGR prefixes of the enclosing tags, innermost first - the order the
 * Haskell built by consing onto the option list. */
struct opts {
    const char *sgr;
    const struct opts *outer;
};

static const char *opts_concat(const struct opts *opts)
{
    sb_t sb = {0};
    for (const struct opts *o = opts; o != nullptr; o = o->outer)
        sb_add(&sb, o->sgr);
    return sb_str(&sb);
}

/* Flattens a <repr> subtree into text parts.  Character and entity references
 * have become marker elements by now (see mark_refs), matching the Haskell
 * that printed nothing for them. */
static void repr_parts(parts_t *out, const xmlNode *node, const struct opts *opts,
                       terminal_t term)
{
    const char *clear = term.color ? SGR_RESET : "";

    switch (node->type) {
    case XML_ELEMENT_NODE: {
        /* A former character reference: prints nothing, but it did keep the
         * text on either side in two separate parts. */
        if (xmlStrcmp(node->name, (const xmlChar *)REF_MARKER) == 0)
            break;

        struct opts inner = {
            .sgr = tag_to_sgr((const char *)node->name, term.color),
            .outer = opts,
        };
        for (const xmlNode *c = node->children; c != nullptr; c = c->next)
            repr_parts(out, c, &inner, term);
        break;
    }
    case XML_TEXT_NODE:
    case XML_CDATA_SECTION_NODE:
        parts_push(out, (textpart_t){
                            .opts = opts_concat(opts),
                            .s = (const char *)node->content,
                            .end = clear,
                        });
        break;
    case XML_COMMENT_NODE:
    case XML_PI_NODE:
        parts_push(out, (textpart_t){
                            .opts = opts_concat(opts),
                            .s = "Misc",
                            .end = clear,
                        });
        break;
    default:
        break;
    }
}

static void render_translation(sb_t *out, terminal_t term, const translation_t *entry)
{
    if (entry->unsupported != nullptr) {
        sb_add(out, entry->unsupported);
        return;
    }

    textpart_t sep = { .s = "--" };
    /* The separator plus the space on either side of it. */
    int side_width = (term.width - (2 + (int)part_len(sep))) / 2;

    parts_t left_src = {0};
    parts_t right_src = {0};
    repr_parts(&left_src, entry->left, nullptr, term);
    repr_parts(&right_src, entry->right, nullptr, term);

    lines_t left = wrap_fill_start(side_width, &left_src);
    lines_t right = wrap(side_width, &right_src);

    size_t pad = side_width > 0 ? (size_t)side_width : 0;
    char *spaces = xalloc(pad + 1);
    memset(spaces, ' ', pad);
    textpart_t left_default = { .s = spaces };

    size_t rows = left.n > right.n ? left.n : right.n;
    for (size_t i = 0; i < rows; i++) {
        if (i > 0)
            sb_addc(out, '\n');

        parts_t row = {0};
        if (i < left.n)
            for (size_t j = 0; j < left.v[i].n; j++)
                parts_push(&row, left.v[i].v[j]);
        else
            parts_push(&row, left_default);

        parts_push(&row, sep);

        if (i < right.n)
            for (size_t j = 0; j < right.v[i].n; j++)
                parts_push(&row, right.v[i].v[j]);

        show_words(out, &row);
    }
}

const char *render_part(terminal_t term, const part_t *part)
{
    sb_t out = {0};

    switch (part->kind) {
    case PART_SECTION:
        sb_addf(&out, "%s: %s\n", part->direct == DIRECT ? "Direct" : "Indirect",
                part->title);
        /* leo lists the best match last, so entries are printed backwards. */
        for (size_t i = part->nentries; i > 0; i--) {
            render_translation(&out, term, &part->entries[i - 1]);
            sb_addc(&out, '\n');
        }
        break;

    case PART_SIMILAR: {
        const char *clear = term.color ? SGR_RESET : "";
        sb_addf(&out, "%s%s:%s ", term.color ? SGR_DULL_WHITE : "", part->lang, clear);
        for (size_t i = 0; i < part->nwords; i++) {
            if (i > 0)
                sb_add(&out, " - ");
            sb_addf(&out, "%s%s%s", term.color ? SGR_DULL_RED : "", part->words[i],
                    clear);
        }
        break;
    }

    case PART_UNSUPPORTED:
        sb_addf(&out, "UNSUPPORTED_PART %s", part->text);
        break;
    }

    return sb_str(&out);
}
