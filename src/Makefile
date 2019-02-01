ERLC=erlc
ERLC_OPTS=+export_all -o ../ebin

# There is a make rule for this somewhere...

OBJDIR = ../ebin

SRC := $(wildcard ./*.erl)
INC := $(wildcard ./*.hrl)
OBJ := $(SRC:./%.erl=$(OBJDIR)/%.beam)

all: $(OBJ)

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

clean:
	-rm -f ../ebin/*

