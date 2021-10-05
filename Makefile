
all:
	mkdir -p ./ebin/
	(cd src ; make all)

clean:
	(cd src ; make clean)
