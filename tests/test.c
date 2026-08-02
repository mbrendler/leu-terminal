/* Unit tests.  Every check prints one line; a failing run exits non-zero. */
#include <stdio.h>
#include <string.h>

#include <libxml/parser.h>

#include "../src/model.h"
#include "../src/parse.h"
#include "../src/render.h"
#include "../src/util.h"
#include "../src/wrap.h"

static int failures = 0;

static void check(const char *what, const char *expected, const char *actual)
{
    if (strcmp(expected, actual) == 0) {
        printf("ok   %s\n", what);
    } else {
        printf("FAIL %s\n       expected: <%s>\n       actual:   <%s>\n", what,
               expected, actual);
        failures++;
    }
}

static void check_size(const char *what, size_t expected, size_t actual)
{
    char e[32];
    char a[32];
    snprintf(e, sizeof e, "%zu", expected);
    snprintf(a, sizeof a, "%zu", actual);
    check(what, e, a);
}

/* Splits on spaces, like the Haskell tests' createSimpleParts. */
static parts_t simple_parts(const char *words)
{
    parts_t parts = {0};
    while (*words != '\0') {
        const char *end = strchr(words, ' ');
        if (end == nullptr)
            end = words + strlen(words);
        if (end > words) {
            char *word = xalloc((size_t)(end - words) + 1);
            memcpy(word, words, (size_t)(end - words));
            parts_push(&parts, (textpart_t){ .s = word });
        }
        words = *end == '\0' ? end : end + 1;
    }
    return parts;
}

static const char *wrapped(int width, const char *words)
{
    parts_t parts = simple_parts(words);
    lines_t lines = wrap(width, &parts);
    sb_t out = {0};
    show_lines(&out, &lines);
    return sb_str(&out);
}

static const char *filled(int width, const char *words)
{
    parts_t parts = simple_parts(words);
    lines_t lines = wrap_fill_start(width, &parts);
    sb_t out = {0};
    show_lines(&out, &lines);
    return sb_str(&out);
}

static void test_wrap(void)
{
    /* The four assertions the Haskell Tests.hs had. */
    check("wrap: chops an overlong word", "12\n34\n5", wrapped(2, "12345"));
    check("wrap: greedy fill", "1234\n78\n90a a", wrapped(5, "1234 78 90a a"));
    check("fill: pads a short line", "  12", filled(4, "12"));
    check("fill: pads every line", "  12\n 345", filled(4, "12 345"));

    check("wrap: no input is one empty line", "", wrapped(10, ""));
    check("wrap: a word of exactly the width fits", "abc", wrapped(3, "abc"));
    check("wrap: two words of exactly the width", "ab\ncd", wrapped(2, "ab cd"));
    check("wrap: a word of twice the width", "ab\ncd\nef", wrapped(2, "abcdef"));
    check("wrap: width one", "a\nb", wrapped(1, "ab"));
    check("wrap: chops on codepoint boundaries", "äö\nü",
          wrapped(2, "äöü"));

    /* The pad is empty here, but the space that joins it to the text still
     * counts - dropping the empty part would lose a column. */
    check("fill: zero length pad still separates", " 123", filled(4, "123"));
    check("fill: exact fit is untouched", "1234", filled(4, "1234"));
    check("fill: chopped remainders are padded too", "1234\n   5", filled(4, "12345"));
    check("fill: no input", "   ", filled(4, ""));

    /* A terminal narrower than the separator makes the column width zero or
     * negative; the Haskell spun forever there, one character per line is at
     * least progress. */
    check("wrap: zero width", "a\nb", wrapped(0, "ab"));
    check("wrap: negative width", "a\nb", wrapped(-3, "ab"));

    parts_t two = simple_parts("ab cd");
    check_size("parts_len counts the joining space", 5, parts_len(&two));
    parts_t none = {0};
    check_size("parts_len of nothing", 0, parts_len(&none));
    check_size("part_len counts codepoints", 3,
               part_len((textpart_t){ .s = "äöü" }));
}

static void test_utf8(void)
{
    check_size("utf8_len ascii", 5, utf8_len("hello"));
    check_size("utf8_len empty", 0, utf8_len(""));
    check_size("utf8_len two byte", 3, utf8_len("äöü"));
    check_size("utf8_len three byte", 2, utf8_len("你好"));
    check_size("utf8_len four byte", 1, utf8_len("\U0001f600"));

    check("utf8_take ascii", "hel", utf8_take("hello", 3));
    check("utf8_take past the end", "hi", utf8_take("hi", 10));
    check("utf8_take none", "", utf8_take("hi", 0));
    check("utf8_take on a boundary", "äö", utf8_take("äöü", 2));
    check("utf8_take four byte", "\U0001f600", utf8_take("\U0001f600x", 1));
    check("utf8_skip", "llo", utf8_skip("hello", 2));
}

static void test_mark_refs(void)
{
    check("marks: numeric reference", "a<leuref/>b", mark_refs("a&#160;b"));
    check("marks: hex reference", "a<leuref/>b", mark_refs("a&#xa0;b"));
    check("marks: named reference", "a<leuref/>b", mark_refs("a&amp;b"));
    check("marks: adjacent references", "a<leuref/><leuref/>b",
          mark_refs("a&#160;&#160;b"));
    check("marks: dropped inside a tag", "<x t=\"ab\"/>", mark_refs("<x t=\"a&amp;b\"/>"));
    check("marks: a bare ampersand survives", "a & b", mark_refs("a & b"));
    check("marks: an unterminated reference survives", "a&foo", mark_refs("a&foo"));
    check("marks: an empty reference survives", "&;", mark_refs("&;"));
    check("marks: nothing to do", "plain text", mark_refs("plain text"));
}

static void test_languages(void)
{
    check_size("eight languages", 8, nlanguages);
    check("default is english", "ende", lang_lookup(nullptr)->code);
    check("exact match", "frde", lang_lookup("frde")->code);
    check("case insensitive", "chde", lang_lookup("ChDe")->code);
    check("unknown falls back", "ende", lang_lookup("xxde")->code);
    check("empty falls back", "ende", lang_lookup("")->code);
}

static void test_tag_colors(void)
{
    check("tag b", "\033[94m", tag_to_sgr("b", true));
    check("tag small", "\033[33m", tag_to_sgr("small", true));
    check("tag domain", "\033[37m", tag_to_sgr("domain", true));
    check("tag i is silent", "", tag_to_sgr("i", true));
    check("unknown tag is reported", "UNHANDLED TAGNAME (zzz)", tag_to_sgr("zzz", true));
    check("no colour, no codes", "", tag_to_sgr("b", false));
    check("no colour, no report either", "", tag_to_sgr("zzz", false));
}

static partlist_t parse_string(const char *xml)
{
    return parse_document(xml);
}

static void test_parse(void)
{
    partlist_t empty = parse_string("<other/>");
    check_size("a foreign root yields nothing", 0, empty.n);

    partlist_t ignored = parse_string("<xml><advMedia/><forum/><seetab/></xml>");
    check_size("ignored elements yield nothing", 0, ignored.n);

    partlist_t sections = parse_string(
        "<xml><sectionlist><section sctTitle=\"Verben\">"
        "<entry><side><repr>go</repr></side><side><repr>gehen</repr></side></entry>"
        "<minprio/>"
        "</section></sectionlist></xml>");
    check_size("one section is one part", 1, sections.n);
    check("the section title", "Verben", sections.v[0].title);
    check_size("minprio is not an entry", 1, sections.v[0].nentries);
    check_size("a sectionlist is direct", DIRECT, (size_t)sections.v[0].direct);

    partlist_t indirect = parse_string(
        "<xml><part direct=\"0\"><section sctTitle=\"T\"/></part></xml>");
    check_size("direct=\"0\" is indirect", INDIRECT, (size_t)indirect.v[0].direct);

    partlist_t direct = parse_string(
        "<xml><part direct=\"1\"><section sctTitle=\"T\"/></part></xml>");
    check_size("direct=\"1\" is direct", DIRECT, (size_t)direct.v[0].direct);

    /* This shape crashed the Haskell version: whitespace between elements. */
    partlist_t spaced = parse_string("<xml>\n  <part direct=\"1\">\n  </part>\n</xml>");
    check_size("whitespace between elements is skipped", 0, spaced.n);

    partlist_t similar = parse_string(
        "<xml><similar><side lang=\"en\"><word>one</word><word>two</word></side>"
        "</similar></xml>");
    check_size("one side is one part", 1, similar.n);
    check("the side language", "en", similar.v[0].lang);
    check_size("both words", 2, similar.v[0].nwords);
    check("the second word", "two", similar.v[0].words[1]);
}

static void test_render(void)
{
    partlist_t parts = parse_string(
        "<xml><sectionlist><section sctTitle=\"Verben\">"
        "<entry><side><repr><b>go</b></repr></side>"
        "<side><repr>gehen</repr></side></entry>"
        "</section></sectionlist></xml>");

    /* At width 80 each side is (80 - 4) / 2 = 38 columns, so the two character
     * "go" is preceded by 36 spaces: a 35 space pad and the joining space. */
    sb_t plain = {0};
    sb_addf(&plain, "Direct: Verben\n%36sgo -- gehen\n", "");
    check("a plain section", sb_str(&plain),
          render_part((terminal_t){ .width = 80 }, &parts.v[0]));

    sb_t coloured = {0};
    sb_addf(&coloured, "Direct: Verben\n%36s\033[94mgo\033[m -- gehen\033[m\n", "");
    check("a coloured section", sb_str(&coloured),
          render_part((terminal_t){ .width = 80, .color = true }, &parts.v[0]));

    partlist_t similar =
        parse_string("<xml><similar><side lang=\"de\"><word>eins</word>"
                     "<word>zwei</word></side></similar></xml>");
    check("similar words", "de: eins - zwei",
          render_part((terminal_t){ .width = 80 }, &similar.v[0]));
    check("coloured similar words",
          "\033[37mde:\033[m \033[31meins\033[m - \033[31mzwei\033[m",
          render_part((terminal_t){ .width = 80, .color = true }, &similar.v[0]));

    partlist_t no_words =
        parse_string("<xml><similar><side lang=\"de\"/></similar></xml>");
    check("similar without words keeps the trailing space", "de: ",
          render_part((terminal_t){ .width = 80 }, &no_words.v[0]));
}

int main(void)
{
    LIBXML_TEST_VERSION

    test_wrap();
    test_utf8();
    test_mark_refs();
    test_languages();
    test_tag_colors();
    test_parse();
    test_render();

    printf("\n%s\n", failures == 0 ? "all tests passed" : "TESTS FAILED");
    return failures == 0 ? 0 : 1;
}
