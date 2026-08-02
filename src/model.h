/* The document model: what a leo.org answer is reduced to before printing. */
#ifndef LEU_MODEL_H
#define LEU_MODEL_H

#include <libxml/tree.h>

typedef enum : int {
    DIRECT,
    INDIRECT,
} direct_t;

typedef struct {
    const xmlNode *left;  /* <repr> of the first side, NULL when unsupported */
    const xmlNode *right; /* <repr> of the second side */
    const char *unsupported; /* set instead when the entry was not understood */
} translation_t;

typedef enum : int {
    PART_SECTION,
    PART_SIMILAR,
    PART_UNSUPPORTED,
} part_kind_t;

typedef struct {
    part_kind_t kind;

    /* PART_SECTION */
    direct_t direct;
    const char *title;
    translation_t *entries;
    size_t nentries;

    /* PART_SIMILAR */
    const char **words;
    size_t nwords;
    const char *lang;

    /* PART_UNSUPPORTED */
    const char *text;
} part_t;

typedef struct {
    const char *code;   /* leo's lp parameter, also the -l value */
    const char *pretty; /* the --show-languages line */
} lang_t;

extern const lang_t languages[];
extern const size_t nlanguages;

/* Case-insensitive lookup; unknown names fall back to the first entry. */
const lang_t *lang_lookup(const char *name);

#endif
