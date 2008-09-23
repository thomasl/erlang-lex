# ERLC_FLAGS += -pa ../ebin

# ok, a bit clumsy

all: ../ebin/lex.beam

../ebin/lex.beam:
	erlc -o ../ebin lex.erl

clean:
	-rm -f ../ebin/*

