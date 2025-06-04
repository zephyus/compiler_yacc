/*lex*/

%{
#include <stdio.h>
#include <string.h>   // strcmp, strdup, strncpy
#include <stdlib.h>   // malloc, atof, atoi, strtol
#include <ctype.h>    // isprint

// Ensure YYLTYPE is defined before B113040015.tab.h if it's not self-contained
// For Bison 3.0+ YYLTYPE is usually defined in the .tab.h if %locations is used.
#ifndef YYLTYPE_IS_DECLARED
#define YYLTYPE_IS_DECLARED 1
typedef struct YYLTYPE {
    int first_line;
    int first_column;
    int last_line;
    int last_column;
} YYLTYPE;
#endif

#include "B113040015.tab.h" // For token definitions AND YYLTYPE from Bison


/* 改名，避免跟 Parser 的 yyerror 衝突 */
void yyerror_lexer(const char *s) {
    fprintf(stderr, "Lexical Error: %s (at line %d, near text '%s')\n",
            s, yylineno, yytext);
}


/* 字串常量緩衝 */
#define MAX_STR_CONST 1024
static char string_buf[MAX_STR_CONST];
static int string_buf_ptr;
static int string_start_line, string_start_char_column;

/* 多行註解起始行號 */
static int ml_comment_start_line;

/* 字元常量緩衝 */
#define CHAR_BUF_MAX_LEN 8
static char char_val_buf[CHAR_BUF_MAX_LEN];
static int char_buf_ptr;

/* 欄位追蹤 */
int charCount = 1; // Represents the starting column of the current token

/* YY_USER_ACTION to set location data */
#define YY_USER_ACTION \
    yylloc.first_line = yylineno; \
    yylloc.first_column = charCount; \
    yylloc.last_line = yylineno; \
    yylloc.last_column = charCount + yyleng - 1;

/* 簡易識別字表 */
#define MAX_ID_TABLE_SIZE 100
static int id_table_count = 0;
static char *identifier_symbol_table[MAX_ID_TABLE_SIZE];

void create_symbol_table() {
    id_table_count = 0;
    for (int i = 0; i < MAX_ID_TABLE_SIZE; i++)
        identifier_symbol_table[i] = NULL;
}

int lookup_identifier(char *s) {
    for (int i = 0; i < id_table_count; i++)
        if (identifier_symbol_table[i] && strcmp(identifier_symbol_table[i], s) == 0)
            return i;
    return -1;
}

void insert_identifier(char *s) {
    if (id_table_count < MAX_ID_TABLE_SIZE && lookup_identifier(s) == -1) {
        identifier_symbol_table[id_table_count++] = strdup(s);
        if (!identifier_symbol_table[id_table_count-1])
            yyerror_lexer("Memory allocation failed for identifier");
    }
}

/* 處理跳脫字元 */
char *process_escapes(const char *buf, int len) {
    char *out = (char*)malloc(len+1);
    if (!out) { yyerror_lexer("Memory allocation error"); return NULL; }
    int i = 0, j = 0;
    while (i < len) {
        if (buf[i] == '\\' && i+1 < len) {
            i++;
            switch (buf[i]) {
                case 'n': out[j++] = '\n'; break;
                case 't': out[j++] = '\t'; break;
                case 'r': out[j++] = '\r'; break;
                case 'b': out[j++] = '\b'; break;
                case 'f': out[j++] = '\f'; break;
                case '\\':out[j++] = '\\'; break;
                case '\'':out[j++] = '\''; break;
                case '"': out[j++] = '"' ; break;
                default:
                    out[j++] = '\\';
                    out[j++] = buf[i];
            }
            i++;
        } else {
            out[j++] = buf[i++];
        }
    }
    out[j] = '\0';
    return out;
}
%}

%option noyywrap
%option yylineno
%option nounput noinput

%x STRING_STATE CHAR_STATE ML_COMMENT

%%

<INITIAL>{
    /* 空白與換行 */
    [ \t]+           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; }
    \n               { /* YY_USER_ACTION implicitly called */ charCount = 1; /* yylineno is auto-incremented by flex */ }

    /* 註解 */
    "//"[^\n]*       { /* YY_USER_ACTION implicitly called */ charCount += yyleng; }
    "/\*"            { /* YY_USER_ACTION implicitly called */ ml_comment_start_line = yylineno; BEGIN(ML_COMMENT); charCount += yyleng; }

    /* 關鍵字 */
    "boolean"        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_BOOLEAN; }
    "break"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_BREAK; }
    "byte"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_BYTE; }
    "case"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CASE; }
    "char"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CHAR; }
    "catch"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CATCH; }
    "class"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CLASS; }
    "const"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CONST; }
    "continue"       { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_CONTINUE; }
    "default"        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_DEFAULT; }
    "do"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_DO; }
    "double"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_DOUBLE; }
    "else"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_ELSE; }
    "extends"        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_EXTENDS; }
    "false"          { /* YY_USER_ACTION implicitly called */ yylval.intVal = 0; charCount += yyleng; return KW_FALSE; }
    "final"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_FINAL; }
    "finally"        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_FINALLY; }
    "float"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_FLOAT; }
    "for"            { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_FOR; }
    "if"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_IF; }
    "implements"     { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_IMPLEMENTS; }
    "int"            { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_INT; }
    "long"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_LONG; }
    "new"            { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_NEW; }
    "print"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_PRINT; }
    "private"        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_PRIVATE; }
    "protected"      { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_PROTECTED; }
    "public"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_PUBLIC; }
    "return"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_RETURN; }
    "read"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_READ; }
    "short"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_SHORT; }
    "static"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_STATIC; }
    "String"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_STRING_TYPE; }
    "switch"         { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_SWITCH; }
    "this"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_THIS; }
    "true"           { /* YY_USER_ACTION implicitly called */ yylval.intVal = 1; charCount += yyleng; return KW_TRUE; }
    "try"            { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_TRY; }
    "void"           { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_VOID; }
    "while"          { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return KW_WHILE; }

    /* 運算子（長者優先） */
    "++"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_PLUSPLUS; }
    "--"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MINUSMINUS; }
    "=="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_EQEQ; }
    "+="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_PLUSEQ; }
    "-="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MINUSEQ; }
    "*="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MULTEQ; }
    "/="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_DIVEQ; }
    "!="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_NOTEQ; }
    "<="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_LTEQ; }
    ">="             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_GTEQ; }
    "&&"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_ANDAND; }
    "||"             { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_OROR; }
    "="              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_ASSIGN; }
    "+"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_PLUS; }
    "-"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MINUS; }
    "*"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MULT; }
    "/"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_DIV; }
    "%"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_MOD; }
    "!"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_NOT; }
    "<"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_LT; }
    ">"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return OP_GT; }

    /* 符號 */
    ","              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_COMMA; }
    ":"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_COLON; }
    ";"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_SEMICOLON; }
    "("              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_LPAREN; }
    ")"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_RPAREN; }
    "["              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_LBRACKET; }
    "]"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_RBRACKET; }
    "{"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_LBRACE; }
    "}"              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_RBRACE; }
    "."              { /* YY_USER_ACTION implicitly called */ charCount += yyleng; return SYM_DOT; }

    /* 識別字 */
    [A-Za-z_][A-Za-z0-9_$]* {
        /* YY_USER_ACTION implicitly called */
        yylval.strVal = strdup(yytext);
        insert_identifier(yytext); // Assuming this is for semantic analysis, not directly related to location
        charCount += yyleng;
        return IDENTIFIER;
    }

    /* 數值常量：先處理 16 進位 */
    -?0[xX][0-9A-Fa-f]+[lL]? {
        /* YY_USER_ACTION implicitly called */
        yylval.intVal = (int)strtol(yytext, NULL, 0);
        charCount += yyleng;
        return INTEGER_CONSTANT;
    }
    /* 浮點：尾點可有可無指數 */
    -?[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fF]? {
        /* YY_USER_ACTION implicitly called */
        yylval.floatVal = atof(yytext);
        charCount += yyleng;
        return FLOAT_CONSTANT;
    }
    -?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[fF]? {
        /* YY_USER_ACTION implicitly called */
        yylval.floatVal = atof(yytext);
        charCount += yyleng;
        return FLOAT_CONSTANT;
    }
    /* 純指數形式 */
    -?[0-9]+([eE][+-]?[0-9]+)[fF]? {
        /* YY_USER_ACTION implicitly called */
        yylval.floatVal = atof(yytext);
        charCount += yyleng;
        return FLOAT_CONSTANT;
    }
    /* 十進位整數 */
    -?[0-9]+ {
        /* YY_USER_ACTION implicitly called */
        yylval.intVal = atoi(yytext);
        charCount += yyleng;
        return INTEGER_CONSTANT;
    }

    /* 字串起始 */
    \" {
        /* YY_USER_ACTION implicitly called */
        string_buf_ptr = 0;
        string_start_line = yylineno;
        string_start_char_column = charCount; // Column before the quote
        BEGIN(STRING_STATE);
        charCount++; // Advance for the quote itself
    }
    /* 字元起始 */
    \' {
        /* YY_USER_ACTION implicitly called */
        char_buf_ptr = 0;
        string_start_line = yylineno;
        string_start_char_column = charCount; // Column before the quote
        BEGIN(CHAR_STATE);
        charCount++; // Advance for the quote itself
    }

    /* 其他非法字元 */
    . {
        /* YY_USER_ACTION implicitly called */
        fprintf(stderr, "Line %d, Column %d: Lexical Error: Unrecognized character '%s'\n",
                yylineno, charCount, yytext); // charCount here is start of bad char
        charCount += yyleng;
    }
}

<ML_COMMENT>{
    [^*\n]+        { /* YY_USER_ACTION implicitly called */ charCount += yyleng; }
    "*"+[^*/\n]+   { /* YY_USER_ACTION implicitly called */ charCount += yyleng; }
    \n             { /* YY_USER_ACTION implicitly called */ charCount = 1; }
    "*"+"/"        { /* YY_USER_ACTION implicitly called */ BEGIN(INITIAL); charCount += yyleng; }
    <<EOF>>        {
        fprintf(stderr,
            "Line %d, Column %d: Lexical Error: Unterminated multi-line comment starting near line %d\n",
            yylineno, charCount, ml_comment_start_line);
        BEGIN(INITIAL);
        yyterminate();
    }
}

<STRING_STATE>{
    \" {
        /* YY_USER_ACTION implicitly called for the quote itself if needed, but charCount is tricky here */
        // The charCount for STRING_LITERAL should be the start of the opening quote.
        // YY_USER_ACTION for the STRING_LITERAL token itself will be set by Bison based on yylloc of opening quote.
        string_buf[string_buf_ptr] = '\0';
        yylval.strVal = process_escapes(string_buf, string_buf_ptr);
        BEGIN(INITIAL);
        // yylloc for STRING_LITERAL should refer to the location of the opening quote.
        // This is handled by setting yylloc before returning the token.
        // The charCount here is at the position *after* the closing quote.
        yylloc.first_line = string_start_line; // Use stored start line
        yylloc.first_column = string_start_char_column; // Use stored start column
        yylloc.last_line = yylineno;
        yylloc.last_column = charCount -1; // Column of the closing quote
        return STRING_LITERAL;
    }
    \n {
        // ... existing error message ...
        // charCount already reset by \n rule if it were in INITIAL
        // Here, we are in STRING_STATE, so newline means error.
        // yylloc for the error token (implicit)
        yylloc.first_line = yylineno; 
        yylloc.first_column = charCount;
        yylloc.last_line = yylineno;
        yylloc.last_column = charCount;
        charCount = 1; // Reset for next line
        BEGIN(INITIAL); // Go back to initial state
        // No token returned, error already printed.
    }
    <<EOF>> {
        fprintf(stderr,
            "Line %d, Column %d: Lexical Error: Unterminated string literal (EOF encountered).\n",
            string_start_line, string_start_char_column);
        BEGIN(INITIAL);
        yyterminate();
    }
    \\[^\n] {
        // ...existing code...
        charCount += yyleng; // Track column advance within string
    }
    [^"\\\n]+ {
        // ...existing code...
        charCount += yyleng; // Track column advance within string
    }
}

<CHAR_STATE>{
    \' {
        // Similar to STRING_STATE, set yylloc for the CHAR_LITERAL token
        char_val_buf[char_buf_ptr] = '\0';
        char *tmp = process_escapes(char_val_buf, char_buf_ptr);
        if (tmp && strlen(tmp)==1) {
            yylval.strVal = tmp;
        } else {
            yyerror_lexer("Character literal must resolve to a single char");
            free(tmp);
            yylval.strVal = NULL;
        }
        BEGIN(INITIAL);
        yylloc.first_line = string_start_line; // Use stored start line
        yylloc.first_column = string_start_char_column; // Use stored start column
        yylloc.last_line = yylineno;
        yylloc.last_column = charCount-1; // Column of the closing quote
        return CHAR_LITERAL;
    }
    \n {
        // ... existing error message ...
        yylloc.first_line = yylineno;
        yylloc.first_column = charCount;
        yylloc.last_line = yylineno;
        yylloc.last_column = charCount;
        charCount = 1;
        BEGIN(INITIAL);
    }
    <<EOF>> {
        fprintf(stderr,
            "Line %d, Column %d: Lexical Error: Unterminated character literal (EOF encountered).\n",
            string_start_line, string_start_char_column);
        BEGIN(INITIAL);
        yyterminate();
    }
    \\[^\n] {
        // ...existing code...
        charCount += yyleng;
    }
    [^'\\\n] {
        // ...existing code...
        charCount += yyleng;
    }
    .|\n {
        yyerror_lexer("Malformed character literal");
        if (yytext[0]=='\n') charCount=1; else charCount+=yyleng;
        BEGIN(INITIAL);
    }
}

%%

/* 取得 token 名稱，用於輸出 */
const char* get_token_name(int id) {
    static char s[4] = "' '";
    switch(id) {
        case KW_INT:           return "KW_INT";
        case KW_FLOAT:         return "KW_FLOAT";
        case KW_BOOLEAN:       return "KW_BOOLEAN";
        case KW_CHAR:          return "KW_CHAR";
        case KW_STRING_TYPE:   return "KW_STRING_TYPE";
        case KW_CLASS:         return "KW_CLASS";
        case KW_IF:            return "KW_IF";
        case KW_ELSE:          return "KW_ELSE";
        case KW_WHILE:         return "KW_WHILE";
        case KW_FOR:           return "KW_FOR";
        case KW_RETURN:        return "KW_RETURN";
        case KW_PRINT:         return "KW_PRINT";
        case KW_READ:          return "KW_READ";
        case KW_STATIC:        return "KW_STATIC";
        case KW_FINAL:         return "KW_FINAL";
        case KW_VOID:          return "KW_VOID";
        case KW_NEW:           return "KW_NEW";
        case KW_PUBLIC:        return "KW_PUBLIC";
        case KW_PROTECTED:     return "KW_PROTECTED";
        case KW_PRIVATE:       return "KW_PRIVATE";
        case KW_BREAK:         return "KW_BREAK";
        case KW_BYTE:          return "KW_BYTE";
        case KW_CASE:          return "KW_CASE";
        case KW_CATCH:         return "KW_CATCH";
        case KW_CONST:         return "KW_CONST";
        case KW_CONTINUE:      return "KW_CONTINUE";
        case KW_DEFAULT:       return "KW_DEFAULT";
        case KW_DO:            return "KW_DO";
        case KW_DOUBLE:        return "KW_DOUBLE";
        case KW_EXTENDS:       return "KW_EXTENDS";
        case KW_FALSE:         return "KW_FALSE";
        case KW_FINALLY:       return "KW_FINALLY";
        case KW_IMPLEMENTS:    return "KW_IMPLEMENTS";
        case KW_LONG:          return "KW_LONG";
        case KW_SHORT:         return "KW_SHORT";
        case KW_SWITCH:        return "KW_SWITCH";
        case KW_THIS:          return "KW_THIS";
        case KW_TRUE:          return "KW_TRUE";
        case KW_TRY:           return "KW_TRY";

        case IDENTIFIER:       return "IDENTIFIER";
        case INTEGER_CONSTANT: return "INTEGER_CONSTANT";
        case FLOAT_CONSTANT:   return "FLOAT_CONSTANT";
        case STRING_LITERAL:   return "STRING_LITERAL";
        case CHAR_LITERAL:     return "CHAR_LITERAL";

        case OP_PLUSPLUS:      return "OP_PLUSPLUS";
        case OP_MINUSMINUS:    return "OP_MINUSMINUS";
        case OP_EQEQ:          return "OP_EQEQ";
        case OP_PLUSEQ:        return "OP_PLUSEQ";
        case OP_MINUSEQ:       return "OP_MINUSEQ";
        case OP_MULTEQ:        return "OP_MULTEQ";
        case OP_DIVEQ:         return "OP_DIVEQ";
        case OP_NOTEQ:         return "OP_NOTEQ";
        case OP_LTEQ:          return "OP_LTEQ";
        case OP_GTEQ:          return "OP_GTEQ";
        case OP_ANDAND:        return "OP_ANDAND";
        case OP_OROR:          return "OP_OROR";
        case OP_ASSIGN:        return "OP_ASSIGN";
        case OP_PLUS:          return "OP_PLUS";
        case OP_MINUS:         return "OP_MINUS";
        case OP_MULT:          return "OP_MULT";
        case OP_DIV:           return "OP_DIV";
        case OP_MOD:           return "OP_MOD";
        case OP_NOT:           return "OP_NOT";
        case OP_LT:            return "OP_LT";
        case OP_GT:            return "OP_GT";

        case SYM_COMMA:        return "SYM_COMMA";
        case SYM_COLON:        return "SYM_COLON";
        case SYM_SEMICOLON:    return "SYM_SEMICOLON";
        case SYM_LPAREN:       return "SYM_LPAREN";
        case SYM_RPAREN:       return "SYM_RPAREN";
        case SYM_LBRACKET:     return "SYM_LBRACKET";
        case SYM_RBRACKET:     return "SYM_RBRACKET";
        case SYM_LBRACE:       return "SYM_LBRACE";
        case SYM_RBRACE:       return "SYM_RBRACE";
        case SYM_DOT:          return "SYM_DOT";

        default:
            if (id > 0 && id < 256 && isprint(id)) {
                s[1] = (char)id;
                return s;
            }
            return "UNKNOWN_TOKEN";
    }
}

/* 
主程式 
int main(int argc, char *argv[]) {
    if (argc>1) {
        FILE *f = fopen(argv[1],"r");
        if (!f) { perror(argv[1]); return 1; }
        yyin = f;
    }
    printf("--- Lexer Scan Start ---\n");
    int tok;
    while ((tok = yylex())!=0) {
        printf("Line %3d, Col %3d: Type = %-18s (ID=%3d), Text = \"%s\"",
               yylineno, charCount, get_token_name(tok), tok, yytext);
        switch(tok) {
            case IDENTIFIER:
            case STRING_LITERAL:
            case CHAR_LITERAL:
                if (yylval.strVal) printf(", Value: \"%s\"", yylval.strVal);
                break;
            case INTEGER_CONSTANT:
                printf(", Value: %d", yylval.intVal);
                break;
            case FLOAT_CONSTANT:
                printf(", Value: %f", yylval.floatVal);
                break;
        }
        printf("\n");
    }
    printf("--- Lexer Scan End ---\n");
    if (id_table_count) {
        printf("Identifiers:\n");
        for(int i=0;i<id_table_count;i++)
            printf("  %s\n",identifier_symbol_table[i]);
    }
    return 0;
}
*/
