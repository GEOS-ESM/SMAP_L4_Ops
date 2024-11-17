SHELL = /bin/sh

FFLAGS = -extend_source -assume byterecl

LIBS    =
INCLUDE =

PROG     = tavg1hto3h
FILES    = time.F90 LDAS_TAVG_Mod.F90 $(PROG).F90
OBJECTS  = timetag.o ${FILES:.F90=.o}

$(PROG).x: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(PROG).x
	rm -f *.o
	rm -f *.mod

.SUFFIXES: .H .h .F90 .f90 .F .f .c .o

.f90.o:
	$(FC) -c $(FFLAGS) $(INCLUDE) $*.f90

.F90.o:
	$(FC) -c $(FFLAGS) $(INCLUDE) $*.F90

.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDE) $*.c
