#include <tree_sitter/parser.h>
#include <ctype.h>
#include <stdlib.h>

enum TokenType {
    WHITE_SPACES,
    LINE_PREFIX_COMMENT,
    LINE_SUFFIX_COMMENT,
    LINE_COMMENT,
    COMMENT_ENTRY,
    multiline_string,
};

void *tree_sitter_COBOL_external_scanner_create() {
    return NULL;
}

bool is_white_space(int c) {
    return isspace(c) || c == ';' || c == ',';
}

const int number_of_comment_entry_keywords = 11;
char* any_content_keyword[] = {
    "author",
    "installlation",
    "date-written",
    "date-compiled",
    "security",
    // Compiler directives (IBM / MF) often appear before IDENTIFICATION DIVISION.
    // Prevent them from being classified as COMMENT_ENTRY (which would surface as
    // an ERROR node at the top-level).
    "cbl",
    "process",
    "identification division",
    "environment division",
    "data division",
    "procedure division",
};

bool start_with_word( TSLexer *lexer, char *words[], int number_of_words) {
    while(lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        lexer->advance(lexer, true);
    }

    // MSVC does not support C99 variable-length arrays (VLA). Allocate explicitly.
    char **keyword_pointer = (char **)malloc(sizeof(char *) * (size_t)number_of_words);
    bool *continue_check = (bool *)malloc(sizeof(bool) * (size_t)number_of_words);
    if (keyword_pointer == NULL || continue_check == NULL) {
        free(keyword_pointer);
        free(continue_check);
        return false;
    }
    for(int i=0; i<number_of_words; ++i) {
        keyword_pointer[i] = words[i];
        continue_check[i] = true;
    }

    while(true) {
        // At the end of the line
        if(lexer->get_column(lexer) > 71 || lexer->lookahead == '\n' || lexer->lookahead == 0) {
            free(keyword_pointer);
            free(continue_check);
            return false;
        }

        // If all keyword matching fails, move to the end of the line
        bool all_match_failed = true;
        for(int i=0; i<number_of_words; ++i) {
            if(continue_check[i]) {
                all_match_failed = false;
            }
        }

        if(all_match_failed) {
            for(; lexer->get_column(lexer) < 71 && lexer->lookahead != '\n' && lexer->lookahead != 0;
            lexer->advance(lexer, true)) {
            }
            free(keyword_pointer);
            free(continue_check);
            return false;
        }

        // If the head of the line matches any of specified keywords, return true;
        char c = lexer->lookahead;
        for(int i=0; i<number_of_words; ++i) {
            if(*(keyword_pointer[i]) == 0 && continue_check[i]) {
                free(keyword_pointer);
                free(continue_check);
                return true;
            }
        }

        // matching keywords
        for(int i=0; i<number_of_words; ++i) {
            char k = *(keyword_pointer[i]);
            if(continue_check[i]) {
                continue_check[i] = c == toupper(k) || c == tolower(k);
            }
            (keyword_pointer[i])++;
        }

        // next character
        lexer->advance(lexer, true);
    }

    free(keyword_pointer);
    free(continue_check);
    return false;
}

bool tree_sitter_COBOL_external_scanner_scan(void *payload, TSLexer *lexer,
                                            const bool *valid_symbols) {
    if(lexer->lookahead == 0) {
        return false;
    }

    if(valid_symbols[WHITE_SPACES]) {
        if(is_white_space(lexer->lookahead)) {
            while(is_white_space(lexer->lookahead)) {
                lexer->advance(lexer, true);
            }
            lexer->result_symbol = WHITE_SPACES;
            lexer->mark_end(lexer);
            return true;
        }
    }

    if(valid_symbols[LINE_PREFIX_COMMENT] && lexer->get_column(lexer) <= 5) {
        while(lexer->get_column(lexer) <= 5) {
            lexer->advance(lexer, true);
        }
        lexer->result_symbol = LINE_PREFIX_COMMENT;
        lexer->mark_end(lexer);
        return true;
    }

    // Vendor/compiler directives commonly used in enterprise COBOL:
    // - IBM:   >>SOURCE FORMAT FREE|FIXED, >>IF/>>ELSE/>>END-IF, etc.
    // - MF:    $SET ...
    //
    // These directives are not part of COBOL proper and are best handled by
    // the consuming pipeline preprocessor, but the grammar must tolerate them
    // to avoid ERROR cascades. We conservatively treat directive lines as
    // comment lines when they appear at the beginning of the "text area".
    //
    // NOTE: We only trigger on leading '>' or '$' to avoid swallowing valid
    // COBOL code (keywords like PROCEDURE/PROGRAM-ID are ambiguous).
    if (valid_symbols[LINE_COMMENT] && lexer->get_column(lexer) >= 7) {
        if (lexer->lookahead == '>' || lexer->lookahead == '$') {
            while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
                lexer->advance(lexer, true);
            }
            lexer->result_symbol = LINE_COMMENT;
            lexer->mark_end(lexer);
            return true;
        }
    }

    if(valid_symbols[LINE_COMMENT]) {
        if(lexer->get_column(lexer) == 6) {
            if(lexer->lookahead == '*' || lexer->lookahead == '/') {
                while(lexer->lookahead != '\n' && lexer->lookahead != 0) {
                    lexer->advance(lexer, true);
                }
                lexer->result_symbol = LINE_COMMENT;
                lexer->mark_end(lexer);
                return true;
            } else {
                // Consume the indicator column (often a space) and then tolerate
                // directive-style lines that begin with '>>' (IBM) or '$' (MF).
                lexer->advance(lexer, true);
                if (lexer->lookahead == '>' || lexer->lookahead == '$') {
                    while (lexer->lookahead != '\n' && lexer->lookahead != 0) {
                        lexer->advance(lexer, true);
                    }
                    lexer->result_symbol = LINE_COMMENT;
                    lexer->mark_end(lexer);
                    return true;
                }
                lexer->mark_end(lexer);
                return false;
            }
        }
    }

    if(valid_symbols[LINE_SUFFIX_COMMENT]) {
        if(lexer->get_column(lexer) >= 72) {
            while(lexer->lookahead != '\n' && lexer->lookahead != 0) {
                lexer->advance(lexer, true);
            }
            lexer->result_symbol = LINE_SUFFIX_COMMENT;
            lexer->mark_end(lexer);
            return true;
        }
    }

    if(valid_symbols[COMMENT_ENTRY]) {
        if(!start_with_word(lexer, any_content_keyword, number_of_comment_entry_keywords)) {
            lexer->mark_end(lexer);
            lexer->result_symbol = COMMENT_ENTRY;
            return true;
        } else {
            return false;
        }
    }

    if(valid_symbols[multiline_string]) {
        while(true) {
            if(lexer->lookahead != '"') {
                return false;
            }
            lexer->advance(lexer, false);
            while(lexer->lookahead != '"' && lexer->lookahead != 0 && lexer->get_column(lexer) < 72) {
                lexer->advance(lexer, false);
            }
            if(lexer->lookahead == '"') {
                lexer->result_symbol = multiline_string;
                lexer->advance(lexer, false);
                lexer->mark_end(lexer);
                return true;
            }
            while(lexer->lookahead != 0 && lexer->lookahead != '\n') {
                lexer->advance(lexer, true);
            }
            if(lexer->lookahead == 0) {
                return false;
            }
            lexer->advance(lexer, true);
            int i;
            for(i=0; i<=5; ++i) {
                if(lexer->lookahead == 0 || lexer->lookahead == '\n') {
                    return false;
                }
                lexer->advance(lexer, true);
            }

            if(lexer->lookahead != '-') {
                return false;
            }

            lexer->advance(lexer, true);
            while(lexer->lookahead == ' ' && lexer->get_column(lexer) < 72) {
                lexer->advance(lexer, true);
            }
        }
    }

    return false;
}

unsigned tree_sitter_COBOL_external_scanner_serialize(void *payload, char *buffer) {
    return 0;
}

void tree_sitter_COBOL_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

void tree_sitter_COBOL_external_scanner_destroy(void *payload) {
}