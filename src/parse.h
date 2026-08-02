/* Turns a leo.org query.xml answer into a list of parts. */
#ifndef LEU_PARSE_H
#define LEU_PARSE_H

#include "model.h"

typedef struct {
    part_t *v;
    size_t n;
    size_t cap;
} partlist_t;

/* The name of the marker element that replaces a reference, see mark_refs. */
#define REF_MARKER "leuref"

/* Rewrites every character and entity reference in the document, in place.
 *
 * The Haskell renderer printed no output for a reference (Leu/Pretty.hs mapped
 * CRef to the empty list) but still treated the text on either side as two
 * separate words, and leo peppers its markup with &#160;.  Simply deleting the
 * reference would glue those words together, and leaving it alone would make
 * libxml2 substitute it into the text.  So a reference in content becomes an
 * empty <leuref/> element, which renders as nothing yet keeps its neighbours
 * apart; inside a tag, where an element cannot go, it is dropped.
 *
 * This is the one place to change if references should ever be rendered. */
char *mark_refs(const char *doc);

/* The returned parts reference nodes of an xmlDoc that is intentionally never
 * freed, so they stay valid until the process exits. */
partlist_t parse_document(const char *doc);

#endif
