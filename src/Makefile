ERLC=erlc
ERLC_OPTS=+export_all -o ../ebin

OBJDIR = ../ebin

SRC := $(wildcard ./*.erl)
INC := $(wildcard ./*.hrl)
OBJ := $(SRC:./%.erl=$(OBJDIR)/%.beam)

all: $(OBJ)

../ebin/%.beam: %.erl $(INC)
	$(ERLC) $(ERLC_OPTS) $<

clean:
	-rm -f ../ebin/*

