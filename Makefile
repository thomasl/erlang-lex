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
