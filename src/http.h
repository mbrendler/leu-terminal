/* The dict.leo.org query. */
#ifndef LEU_HTTP_H
#define LEU_HTTP_H

#include "model.h"

/* Returns the response body, or exits with a message on a failed request. */
char *http_query(const char *search, const lang_t *lang);

#endif
