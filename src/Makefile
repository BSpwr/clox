IDIR =../include
CC=gcc
CFLAGS=-I$(IDIR)

ODIR=../obj
LDIR =../lib

LIBS=

_DEPS = 
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = main.o chunk.o compiler.o debug.o memory.o scanner.o value.o vm.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))


$(ODIR)/%.o: %.c $(DEPS)
	mkdir -p $(ODIR)
	$(CC) -c -o $@ $< $(CFLAGS)

default: ../clox

../clox: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)
	rm -r $(ODIR)

.PHONY: clean

clean:
	rm -f clox