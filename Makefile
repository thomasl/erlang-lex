UCD_VERSION ?= 17.0.0
UCD_URL = https://www.unicode.org/Public/$(UCD_VERSION)/ucd
UCD_DIR ?= ucd-$(UCD_VERSION)
UCD_FILES = extracted/DerivedGeneralCategory.txt Scripts.txt \
	ScriptExtensions.txt Blocks.txt PropList.txt DerivedCoreProperties.txt \
	PropertyAliases.txt PropertyValueAliases.txt CaseFolding.txt

all:
	mkdir -p ./ebin/
	(cd src ; make all)

test: all
	erl -noshell -pa ebin -eval \
	  'case eunit:test([lex_spec_parse_tests, lex_unicode_tests, lex_examples_tests], []) of ok -> halt(0); _ -> halt(1) end.'

# Regenerate src/lex_unicode_data.erl from the Unicode Character Database
# (downloads the UCD files into $(UCD_DIR) unless they are already there).
unicode-data:
	mkdir -p $(UCD_DIR)/extracted
	for f in $(UCD_FILES); do \
	  test -f $(UCD_DIR)/$$f || curl -sfL -o $(UCD_DIR)/$$f $(UCD_URL)/$$f || exit 1; \
	done
	escript tools/gen_unicode_data.escript $(UCD_DIR) $(UCD_VERSION) src/lex_unicode_data.erl

clean:
	(cd src ; make clean)

.PHONY: all test unicode-data clean

# Compare the DFA table as a tuple of tuples with a binary of 16- and
# 32-bit entries (see doc/README.md, "Table representation").
BENCH_VARIANTS = tuple bin16 bin32
bench-table: all
	@for v in $(BENCH_VARIANTS); do \
	  mkdir -p bench/ebin_$$v; \
	  case $$v in \
	    tuple) defs="" ;; \
	    bin16) defs="-DLEX_BINARY_TABLE -DLEX_TABLE_BITS=16" ;; \
	    bin32) defs="-DLEX_BINARY_TABLE -DLEX_TABLE_BITS=32" ;; \
	  esac; \
	  erlc +export_all $$defs -o bench/ebin_$$v src/lex.erl || exit 1; \
	  erlc -o bench/ebin_$$v bench/lex_table_bench.erl || exit 1; \
	  erl -noshell -pa ebin -pa bench/ebin_$$v -run lex_table_bench run $$v -s init stop || exit 1; \
	done
