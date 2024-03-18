SHELL=/bin/sh

include makefile.${MACHINE_ID}.${COMPILER}

CMD = rdbfmsua.x
OBJS = rdbfmsua.o

$(CMD): $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f ${OBJS}

clobber: clean
	-rm -f ${CMD}

install:
	-mv ${CMD} ${PREFIX}/bin/

