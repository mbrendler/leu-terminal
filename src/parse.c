#include <ctype.h>
#include <string.h>

#include <libxml/parser.h>

#include "parse.h"
#include "util.h"

/* Elements below <xml> that carry nothing we print. */
static const char *const ignored_parts[] = {
    "advMedia", "search",      "forum",    "baseform", "forumRef",
    "servicedata", "ffsynlist", "grammar", "seetab",
};

static bool is_elem(const xmlNode *node)
{
    return node != nullptr && node->type == XML_ELEMENT_NODE;
}

static bool named(const xmlNode *node, const char *name)
{
    return is_elem(node) && xmlStrcmp(node->name, (const xmlChar *)name) == 0;
}

/* Attribute value, or the given fallback when the attribute is absent. */
static const char *attr(const xmlNode *node, const char *name, const char *fallback)
{
    for (const xmlAttr *a = node->properties; a != nullptr; a = a->next)
        if (xmlStrcmp(a->name, (const xmlChar *)name) == 0)
            return a->children != nullptr ? (const char *)a->children->content : "";
    return fallback;
}

static const xmlNode *first_child_named(const xmlNode *node, const char *name)
{
    for (const xmlNode *c = node->children; c != nullptr; c = c->next)
        if (named(c, name))
            return c;
    return nullptr;
}

static void parts_add(partlist_t *list, part_t part)
{
    if (list->n == list->cap) {
        size_t cap = list->cap ? list->cap * 2 : 8;
        part_t *v = xalloc(cap * sizeof *v);
        if (list->v != nullptr)
            memcpy(v, list->v, list->n * sizeof *v);
        list->v = v;
        list->cap = cap;
    }
    list->v[list->n++] = part;
}

char *mark_refs(const char *doc)
{
    constexpr size_t max_ref = 32; /* longer than any reference leo emits */

    sb_t out = {0};
    bool in_tag = false;

    for (const char *in = doc; *in != '\0';) {
        if (*in == '<' || *in == '>') {
            in_tag = *in == '<';
            sb_addc(&out, *in++);
            continue;
        }
        if (*in != '&') {
            sb_addc(&out, *in++);
            continue;
        }

        const char *p = in + 1;
        if (*p == '#') {
            p++;
            if (*p == 'x' || *p == 'X')
                p++;
        }
        while (isalnum((unsigned char)*p) && (size_t)(p - in) < max_ref)
            p++;

        if (*p == ';' && p > in + 1) {
            if (!in_tag)
                sb_add(&out, "<" REF_MARKER "/>");
            in = p + 1;
        } else {
            sb_addc(&out, *in++); /* a lone '&', keep it */
        }
    }
    return out.s ? out.s : xstrdup("");
}

/* Both sides of one <entry>. */
static translation_t entry_to_translation(const xmlNode *entry)
{
    const xmlNode *sides[2] = { nullptr, nullptr };
    size_t found = 0;
    for (const xmlNode *c = entry->children; c != nullptr && found < 2; c = c->next)
        if (is_elem(c))
            sides[found++] = c;

    if (found == 2 && named(sides[0], "side") && named(sides[1], "side")) {
        const xmlNode *left = first_child_named(sides[0], "repr");
        const xmlNode *right = first_child_named(sides[1], "repr");
        if (left != nullptr && right != nullptr)
            return (translation_t){ .left = left, .right = right };
    }
    return (translation_t){ .unsupported = "UNSUPPORTED_TRANSLATION" };
}

/* One <section> becomes one printable part. */
static part_t section_to_part(const xmlNode *section, direct_t direct)
{
    part_t part = {
        .kind = PART_SECTION,
        .direct = direct,
        .title = attr(section, "sctTitle", ""),
    };

    size_t cap = 0;
    for (const xmlNode *c = section->children; c != nullptr; c = c->next) {
        if (!is_elem(c) || named(c, "minprio"))
            continue;
        if (part.nentries == cap) {
            cap = cap ? cap * 2 : 8;
            translation_t *v = xalloc(cap * sizeof *v);
            if (part.entries != nullptr)
                memcpy(v, part.entries, part.nentries * sizeof *v);
            part.entries = v;
        }
        part.entries[part.nentries++] = entry_to_translation(c);
    }
    return part;
}

/* One <side> of the <similar> block. */
static part_t side_to_similar(const xmlNode *side)
{
    part_t part = {
        .kind = PART_SIMILAR,
        .lang = attr(side, "lang", ""),
    };

    size_t cap = 0;
    for (const xmlNode *word = side->children; word != nullptr; word = word->next) {
        if (!named(word, "word"))
            continue;
        for (const xmlNode *t = word->children; t != nullptr; t = t->next) {
            if (t->type != XML_TEXT_NODE)
                continue;
            if (part.nwords == cap) {
                cap = cap ? cap * 2 : 8;
                const char **v = xalloc(cap * sizeof *v);
                if (part.words != nullptr)
                    memcpy(v, part.words, part.nwords * sizeof *v);
                part.words = v;
            }
            part.words[part.nwords++] = (const char *)t->content;
        }
    }
    return part;
}

static bool is_ignored(const xmlNode *node)
{
    for (size_t i = 0; i < sizeof ignored_parts / sizeof ignored_parts[0]; i++)
        if (named(node, ignored_parts[i]))
            return true;
    return false;
}

static const char *serialise(const xmlNode *node)
{
    xmlBufferPtr buf = xmlBufferCreate();
    xmlNodeDump(buf, node->doc, (xmlNodePtr)node, 0, 0);
    const char *s = xstrdup((const char *)xmlBufferContent(buf));
    xmlBufferFree(buf);
    return s;
}

/* Dispatch one child of the document root. */
static void add_parts_for(partlist_t *list, const xmlNode *node)
{
    if (named(node, "part") || named(node, "sectionlist")) {
        /* <sectionlist> has no direct attribute and always counts as direct. */
        direct_t direct = DIRECT;
        if (named(node, "part"))
            direct = strcmp(attr(node, "direct", ""), "1") == 0 ? DIRECT : INDIRECT;

        for (const xmlNode *c = node->children; c != nullptr; c = c->next)
            if (is_elem(c))
                parts_add(list, section_to_part(c, direct));
        return;
    }

    if (named(node, "similar")) {
        for (const xmlNode *c = node->children; c != nullptr; c = c->next)
            if (is_elem(c))
                parts_add(list, side_to_similar(c));
        return;
    }

    if (is_ignored(node))
        return;

    parts_add(list, (part_t){ .kind = PART_UNSUPPORTED, .text = serialise(node) });
}

partlist_t parse_document(const char *doc)
{
    partlist_t list = {0};

    char *marked = mark_refs(doc);
    xmlDoc *parsed = xmlReadMemory(marked, (int)strlen(marked), "", nullptr,
                                   XML_PARSE_NONET | XML_PARSE_RECOVER
                                       | XML_PARSE_NOERROR | XML_PARSE_NOWARNING);
    if (parsed == nullptr)
        return list;

    const xmlNode *root = xmlDocGetRootElement(parsed);
    if (!named(root, "xml"))
        return list; /* not a leo answer - "No translation found." */

    for (const xmlNode *c = root->children; c != nullptr; c = c->next)
        if (is_elem(c))
            add_parts_for(&list, c);

    return list; /* `parsed` stays alive on purpose, the parts point into it */
}
