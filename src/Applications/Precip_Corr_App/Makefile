SHELL = /bin/sh

FPIT: CMAP_A CPCU_B merge

CMAP_A:
	make corrector CORRECTOR=CMAP_A FORCING=GEOS

CPCU_B:
	make corrector CORRECTOR=CPCU_B FORCING=GEOS

CPCU_A_FP:
	make corrector CORRECTOR=CPCU_A FORCING=GEOSFP

merge:
	make corrector CORRECTOR=merge FORCING=CorrFile

FFLAGS = -extend-source -assume byterecl -real-size 64

CORRECTOR=CPCU_B
FORCING=GEOS

BUILD=/discover/nobackup/dao_ops/jardizzo/software/SLES15/GEOSldas/install-SLES15
BASE=$(BASEDIR)/Linux

FC=$(shell $(BASE)/bin/nf-config --fc)
CC=$(shell $(BASE)/bin/nf-config --cc)

INC_ESMF = -I$(BASE)/include/esmf
#LIB_ESMF = -L$(BASE)/lib -lesmf_fullylinked
LIB_ESMF = -L$(BASE)/lib -lesmf

INC_BASE = -I$(BUILD)/include/MAPL.base
LIB_BASE = -L$(BUILD)/lib -lMAPL.base

INC_MAPL = -I$(BUILD)/include/MAPL_cfio_r4
LIB_MAPL = -L$(BUILD)/lib -lMAPL_cfio_r4

LIB_MPEU = -L$(BUILD)/lib -lGMAO_mpeu
INC_MPEU = -I$(BUILD)/include/GMAO_mpeu

LIB_NETCDF := $(shell $(BASE)/bin/nf-config --flibs)
INC_NETCDF = -I$(BASE)/include/netcdf

LIB_HDF5  = -L$(BASE)/lib -lhdf5_fortran -lhdf5hl_fortran -lhdf5 -lhdf5_hl -lmfhdf -ldf -lz -ljpeg
INC_HDF5  = -I$(BASE)/include/hdf5 \
            -I$(BASE)/include/zlib

LIB_SZ  = -L$(BASE)/lib -lsz
INC_SZ  = -I$(BASE)/include/szlib

LIB_CURL  = -L$(BASE)/lib -lcurl
INC_CURL  = -I$(BASE)/include/curl

#LIB_MPI = -L$(MPI_ROOT)/lib -lmpi
#INC_MPI = -I$(MPI_ROOT)/include

LIBS    = $(LIB_ESMF) $(LIB_MPI) $(LIB_BASE) $(LIB_MAPL) \
          $(LIB_MPEU) $(LIB_NETCDF) $(LIB_HDF5) $(LIB_SZ) $(LIB_CURL) -lgpfs

INCLUDE = $(INC_ESMF) $(INC_MPI) $(INC_BASE) $(INC_MAPL) \
          $(INC_MPEU) $(INC_NETCDF) $(INC_HDF5) $(INC_SZ) $(INC_CURL)

FILES    = time.F90 TypesMod.F90 GridMod.F90 RegridMethodMod.F90 \
           LFO$(FORCING)Mod.F90 LFOBaseMod.F90 CMAPFileMod.F90 \
           CPCUFileMod.F90 RegridMod.F90 $(CORRECTOR)_CorrectorMod.F90 \
           correctPrecip.F90
OBJECTS  = timetag.o ${FILES:.F90=.o}

corrector: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(CORRECTOR)_Correct.x
	rm -f *.o
	rm -f *.mod

.SUFFIXES: .H .h .F90 .f90 .F .f .c .o

.f90.o:
	$(FC) -c $(FFLAGS) -DHAS_NETCDF4 $(INCLUDE) -D_$(CORRECTOR) -D_$(FORCING) $*.f90

.F90.o:
	$(FC) -c $(FFLAGS) -DHAS_NETCDF4 $(INCLUDE) -D_$(CORRECTOR) -D_$(FORCING) $*.F90

.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDE) $*.c
