ERLC=erlc
ERLC_OPTS=+export_all -o ../ebin

all: ../ebin/lex.beam

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

clean:
	-rm -f ../ebin/*

