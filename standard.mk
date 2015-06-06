prefix := /usr/local
exec_prefix := ${prefix}
IVC_LIB := ${exec_prefix}/lib
IVC_INC := ${prefix}/include

ifeq ($(THREADED),y)
THR_RT_OPT := -threaded
endif

all: $(BINARIES)

clean::
	rm -f $(BINARIES) *.hi *.o

%: %.hs
	halvm-ghc $(THR_RT_OPT) --make -o $@ $^

%: %.c
	gcc -o $@ $^ -I$(IVC_INC) -lxenctrl -lcrypto -lxenstore $(IVC_LIB)/libIVC.a
