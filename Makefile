########################################
#            SimpleJavaScanner         #
#          ––  rebuild OK  ––          #
########################################

# ── 工具 ────────────────────────────
CXX   := g++          # 同時編 C / C++
LEX   := flex
YACC  := bison

# ── 旗標 ────────────────────────────
CXXFLAGS := -g -Wall           # debug + 所有警告
LDFLAGS  := -lfl               # 連 libfl（flex runtime）

# ── 原始檔 ──────────────────────────
LEX_SRC   := lex_new.l
YACC_SRC  := B113040015.y

# ── 產出檔名 ────────────────────────
YACC_C_OUTPUT := $(YACC_SRC:.y=.tab.c)   # B113040015.tab.c
YACC_HEADER   := $(YACC_SRC:.y=.tab.h)   # B113040015.tab.h
LEX_C_OUTPUT  := lex.yy.c

# ── 物件檔 ──────────────────────────
LEX_OBJ  := $(LEX_C_OUTPUT:.c=.o)
YACC_OBJ := $(YACC_C_OUTPUT:.c=.o)

# ── 最終執行檔 ──────────────────────
TARGET := demo

# ── phony target ────────────────────
.PHONY: all clean

# ── 預設目標 ────────────────────────
all: $(TARGET)

# ── 連結 ────────────────────────────
$(TARGET): $(LEX_OBJ) $(YACC_OBJ)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

# ── Bison ───────────────────────────
$(YACC_C_OUTPUT) $(YACC_HEADER): $(YACC_SRC)
	$(YACC) -d -v -o $(YACC_C_OUTPUT) $(YACC_SRC)

# ── Flex ────────────────────────────
#   lexer 必須在 include path 找到上面那個 $(YACC_HEADER)
$(LEX_C_OUTPUT): $(LEX_SRC) $(YACC_HEADER)
	$(LEX) -o $@ $<

# ── 編譯 object ─────────────────────
$(LEX_OBJ): $(LEX_C_OUTPUT) $(YACC_HEADER)
	$(CXX) $(CXXFLAGS) -x c -c $< -o $@

$(YACC_OBJ): $(YACC_C_OUTPUT) $(YACC_HEADER)
	$(CXX) $(CXXFLAGS) -DYYDEBUG=1 -c $< -o $@

# ── 清理 ────────────────────────────
clean:
	rm -f $(TARGET) $(LEX_C_OUTPUT) $(YACC_C_OUTPUT) $(YACC_HEADER) \
	       $(LEX_OBJ) $(YACC_OBJ) *.o *.output core
