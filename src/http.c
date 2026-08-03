#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <curl/curl.h>

#include "http.h"
#include "util.h"

/* dict.leo.org is behind Cloudflare, which answers requests that do not look
 * like a browser with '403 Forbidden'.  A plausible user-agent alone is not
 * enough, the modern browser headers below are needed as well.  Only 'gzip' is
 * accepted as content encoding - CURLOPT_ACCEPT_ENCODING makes libcurl send
 * the header and decompress the answer; 'br' would not be handled.
 *
 * The request must be HTTP/1.1 as well.  libcurl negotiates HTTP/2 by default,
 * and Cloudflare rejects the resulting fingerprint of Apple's SecureTransport
 * libcurl with '403 Forbidden' no matter which headers are sent. */
static const char *const browser_headers[] = {
    ("User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like "
     "Gecko) Chrome/126.0.0.0 Safari/537.36"),
    "Accept: */*",
    "Accept-Language: de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
    "sec-ch-ua: \"Chromium\";v=\"126\", \"Not.A/Brand\";v=\"24\"",
    "sec-ch-ua-mobile: ?0",
    "sec-ch-ua-platform: \"Linux\"",
    "Sec-Fetch-Dest: empty",
    "Sec-Fetch-Mode: cors",
    "Sec-Fetch-Site: same-origin",
    "Referer: https://dict.leo.org/",
    "X-Requested-With: XMLHttpRequest",
};

static size_t collect(char *data, size_t size, size_t nmemb, void *userdata)
{
    sb_addn((sb_t *)userdata, data, size * nmemb);
    return size * nmemb;
}

[[noreturn]] static void fail(const char *message)
{
    fprintf(stderr, "leu: %s\n", message);
    exit(1);
}

char *http_query(const char *search, const lang_t *lang)
{
    if (curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK)
        fail("could not initialise libcurl");
    CURL *curl = curl_easy_init();
    if (curl == nullptr)
        fail("could not initialise libcurl");

    char *escaped = curl_easy_escape(curl, search, 0);
    sb_t url = {0};
    sb_addf(&url, "https://dict.leo.org/dictQuery/m-vocab/%s/query.xml", lang->code);
    sb_addf(&url, "?lp=%s&search=%s", lang->code, escaped ? escaped : "");
    sb_add(&url, "&side=both&order=basic&partial=show&filtered=-1");

    struct curl_slist *headers = nullptr;
    for (size_t i = 0; i < sizeof browser_headers / sizeof browser_headers[0]; i++)
        headers = curl_slist_append(headers, browser_headers[i]);

    sb_t body = {0};
    curl_easy_setopt(curl, CURLOPT_URL, sb_str(&url));
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1);
    curl_easy_setopt(curl, CURLOPT_ACCEPT_ENCODING, "gzip");
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 10L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, collect);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &body);

    CURLcode rc = curl_easy_perform(curl);
    if (rc != CURLE_OK)
        fail(curl_easy_strerror(rc));

    long status = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &status);
    if (status != 200) {
        sb_t message = {0};
        sb_addf(&message, "dict.leo.org returned HTTP %ld", status);
        fail(sb_str(&message));
    }

    curl_slist_free_all(headers);
    curl_free(escaped);
    curl_easy_cleanup(curl);

    return body.s ? body.s : xstrdup("");
}
