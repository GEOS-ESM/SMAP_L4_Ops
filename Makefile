
#******************************************************************************
# English Name: Makefile
# -------------
#
# Purpose: Top level makefile for compiling and installing data streams for the
# -------- SMAP Level-4 Subsystem.
#
# Language: Unix Make
# ---------
#
# Notes: 1. g5_modules and smaprc must be sourced before invoking this 
# ------    makefile utility: source g5_modules; source smaprc
#
#        2. All three SMAP L4 modules should be checked out from CVS. This
#           includes SMAP_L4_SM, SMAP_L4_C and SMAP_L4_OPS. All three can be
#           checked out simultaneously by referencing the SMAP_L4 modele.
#
#        3. Users should review the user guidies for the SMAP Level-4 subsytem
#           before using this makefile:
#
#           "SMAP Level-4 SPS User's Guide"
#           "SMAP Level-4 Carbon SPS User's Guide"
#           "SMAP Level-4 Soil Moisture SPS User's Guide"
#
# Prerequisites: g5_modules, smaprc
# -------------- 
#
# Usage: make all
# ------ make install STREAM_TYPE=[stream type] STREAM_NAME=[stream name]
#        make install_stream STREAM_TYPE=[stream type] STREAM_NAME=[stream name]
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# all              make target       IN  Make target for compiling all software
#                                        for the three SMAP Level-4 modules:
#
#                                        SMAP_L4_SM - soil moisture model
#                                        SMAP_L4_C - carbon model
#                                        SMAP_L4_OPS - operational system
#
# install_stream   make target       IN  Make target for installing a data
#                                        stream. The data stream must exist
#                                        within the SMAP_L4_OPS module. See
#                                        note-3.
#
# install          make target       IN  Make target for installing all 
#                                        software binaries and utilities
#                                        for the specified data stream. The
#                                        data stream must first be installed
#                                        (see make install_stream).
#
# stream type           string       IN  Specifies the stream type to be 
#                                        tageted. See note-3.
#
# stream name           string       IN  Specifies the stream name to be 
#                                        tageted. See note-3.
#
# return codes         integer      OUT  Make process return codes:
#
#                                        1: g5_modules and/or smaprc not sourced
#                                        2: Stream not installed (cannot install
#                                           software).
#                                        3: Stream already installed (cannot
#                                           install the data stream)
#                                        4: Stream does not exist (cannot
#                                           install the data stream)
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           10/12/2014      J.Ardizzone  created.
#******************************************************************************

INSTALL_DIR = $(SMAP_OPS_DIR)/$(STREAM_TYPE)/$(STREAM_NAME)
STREAM_DIR  = src/Streams/$(STREAM_TYPE)/$(STREAM_NAME)

all: soil_moisture ops carbon

ops:
	@ if [ -z $(SMAP_HOME) ]; then \
	echo "Please source g5_modules and smaprc"; \
        exit 1; fi

	cd src/Applications; make all

soil_moisture:

	@ if [ -z $(SMAP_HOME) ]; then \
	echo "Please source g5_modules and smaprc"; \
        exit 1; fi

	cd ../GEOSldas/src; gmake install

carbon:
	@ if [ -z $(SMAP_HOME) ]; then \
	echo "Please source g5_modules and smaprc"; \
        exit 1; fi

	cd ../UMT_Carbon/src/Shared/Framework/src; make all
	cd ../UMT_Carbon/src/Applications/DAILY_App; make
	cd ../UMT_Carbon/src/Applications/MET_App; make
	cd ../UMT_Carbon/src/Applications/MODIS_App; make

install:

ifeq ($(STREAM_TYPE),SPL4SM)

	$(info Installing Soil Moisture applications)

	@ if [ ! -d $(INSTALL_DIR) ]; then \
	echo "Stream not installed: Use make install_stream first."; \
        exit 2; fi

	mkdir -p $(INSTALL_DIR)/bin
	cp ../GEOSldas/install/bin/g5_modules $(INSTALL_DIR)/bin
	cp ../GEOSldas/install/bin/GEOSldas.x $(INSTALL_DIR)/bin
	cp src/Applications/HDF5_App/insertMetaGroups.x $(INSTALL_DIR)/bin
	cp src/Applications/HDF5_App/insertXMLFile.x $(INSTALL_DIR)/bin
	cp src/Applications/HDF5_App/ldas2daac.x $(INSTALL_DIR)/bin
	cp src/Applications/L4_SM_App/gen_qa.py $(INSTALL_DIR)/bin
	cp src/Applications/L4_SM_App/prcntl.py $(INSTALL_DIR)/bin
	cp src/Applications/L4_SM_App/link_ldas.py $(INSTALL_DIR)/bin
	cp src/Applications/L4_SM_App/L4_SM.d $(INSTALL_DIR)/bin/$(STREAM_NAME).d
	cp src/SMAP_Shared/saxon/saxon9he.jar $(INSTALL_DIR)/bin
	cp src/SMAP_Shared/saxon/saxon9-xqj.jar $(INSTALL_DIR)/bin
	cp src/SMAP_Shared/saxon/saxon9-unpack.jar $(INSTALL_DIR)/bin
	
endif

ifeq ($(STREAM_TYPE),SPL4C)

	$(info Installing Carbon applications)

	@ if [ ! -d $(INSTALL_DIR) ]; then \
        echo "Stream not installed: Use make install_stream first."; \
        exit 2; fi

	mkdir -p $(INSTALL_DIR)/bin
	cp g5_modules $(INSTALL_DIR)/bin
	cp ../UMT_Carbon/src/Applications/MET_App/l4c_MET.ex $(INSTALL_DIR)/bin
	cp ../UMT_Carbon/src/Applications/MODIS_App/l4c_MODIS.ex $(INSTALL_DIR)/bin
	cp ../UMT_Carbon/src/Applications/DAILY_App/l4c_DAILY.ex $(INSTALL_DIR)/bin
	cp src/Applications/HDF5_App/insertMetaGroups.x $(INSTALL_DIR)/bin
	cp src/Applications/HDF5_App/insertXMLFile.x $(INSTALL_DIR)/bin
	cp src/Applications/L4_C_App/L4_C.d $(INSTALL_DIR)/bin/$(STREAM_NAME).d
	cp src/SMAP_Shared/saxon/saxon9he.jar $(INSTALL_DIR)/bin
	cp src/SMAP_Shared/saxon/saxon9-xqj.jar $(INSTALL_DIR)/bin
	cp src/SMAP_Shared/saxon/saxon9-unpack.jar $(INSTALL_DIR)/bin

endif

install_stream:

	$(info Installing data stream)

	@ if [ -d $(INSTALL_DIR) ]; then \
        echo "Stream already installed."; \
        exit 3; fi

	@ if [ ! -d $(STREAM_DIR) ]; then \
        echo "Stream does not exist."; \
        exit 4; fi

	mkdir -p $(SMAP_OPS_DIR)/$(STREAM_TYPE)
	cd $(SMAP_OPS_DIR)/$(STREAM_TYPE); \
	cvs co -r GEOSldas-SMAP_L4_OPS-R17 -d $(STREAM_NAME) $(SMAP_REPOSITORY)/$(STREAM_DIR)
