.PHONY: all clean veryclean

SHELL = /bin/sh

FFLAGS = -extend_source -assume byterecl

BUILD=/home/dao_ops/SMAP/GEOSldas-SLES12/install
BASE=$(BASEDIR)/Linux

#BASE=/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.4-SLES12/x86_64-unknown-linux-gnu/ifort_18.0.5.274-intelmpi_19.1.0.166/Linux
#BUILD=/home/dao_ops/SMAP/GEOSldas-SLES12/install
MPI_ROOT=/usr/local/intel/2020/compilers_and_libraries_2020.0.166/linux/mpi

FC=mpiifort
CC=mpiicc

INC_ESMF = -I$(BASE)/include/esmf
LIB_ESMF = -L$(BASE)/lib -lesmf_fullylinked

INC_BASE = -I$(BUILD)/include/MAPL_Base
LIB_BASE = -L$(BUILD)/lib -lMAPL_Base

INC_MAPL = -I$(BUILD)/include/MAPL_cfio_r4
LIB_MAPL = -L$(BUILD)/lib -lMAPL_cfio_r4

LIB_MPEU = -L$(BUILD)/lib -lGMAO_mpeu
INC_MPEU = -I$(BUILD)/include/GMAO_mpeu

LIB_LDAS = -L$(BUILD)/lib -lGEOS_LdasShared
INC_LDAS = -I$(BUILD)/include/GEOS_LdasShared

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

LIBS    = $(LIB_ESMF) $(LIB_MPI) $(LIB_BASE) $(LIB_LDAS) $(LIB_MAPL) \
          $(LIB_MPEU) $(LIB_NETCDF) $(LIB_HDF5) $(LIB_SZ) $(LIB_CURL) -lgpfs

INCLUDE = $(INC_ESMF) $(INC_MPI) $(INC_BASE) $(INC_LDAS) $(INC_MAPL) \
          $(INC_MPEU) $(INC_NETCDF) $(INC_HDF5) $(INC_SZ) $(INC_CURL)

PROG     = ldas2daac
FILES    = typesMod.F90 time.F90 GPD_Mod.F90 SDS_Mod.F90 tileMod.F90 \
           interfaceMod.F90 HDF5AttributeMod.F90 HDF5FormatMod.F90 \
           insertMetaGroupsMod.F90 LDAS_Mod.F90 $(PROG).F90 
OBJECTS  = timetag.o ShaveMantissa.o ${FILES:.F90=.o}

$(PROG).x: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(PROG).x
	rm -f *.o
	rm -f *.mod

all:
	make PROG=ldas2daac
	make PROG=insertMetaGroups
	make PROG=insertXMLFile

clean:
	rm -f *.o *.mod

veryclean: clean
	rm -f *.x

.SUFFIXES: .H .h .F90 .f90 .F .f .c .o

.f90.o:
	$(FC) -c $(FFLAGS) -DHAS_NETCDF4 $(INCLUDE) $*.f90

.F90.o:
	$(FC) -c $(FFLAGS) -DHAS_NETCDF4 $(INCLUDE) $*.F90

.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDE) $*.c
