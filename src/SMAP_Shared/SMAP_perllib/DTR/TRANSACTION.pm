                        ##############################
                        #                            #
                        #  PACKAGE DTR::TRANSACTION  #
                        #                            #
                        ##############################
# Use "perldoc DTR::TRANSACTION.pm" to retrieve documentation for this module.
#******************************************************************************

=head1 NAME

DTR::TRANSACTION - Data Transaction

=head1 DESCRIPTION

Parent class for all data transaction (DTR) sub-classes. This module provides
the primary methods to be inherited and/or overridden by it's sub-classes. The methods supplied by this module implement the core functionality for performing data transactions.

=head1 SYNOPSIS

 $pathname = $dtr->genPDR()
 $pathname = $dtr->exportPDR($pdr)
 @pdr = $dtr->getPDR();

=cut


#******************************************************************************
package DTR::TRANSACTION;
#******************************************************************************
# English Name: Data Transaction (DTR) Module
# -------------
#
# Purpose: Provides methods that implement the primary user interface for
# -------- performing data transactions.
#
# Language: Perl
# ---------
#
# Prerequisite: $dtr = DTR->new($fname,%config)
# -------------
#
# Usage:
# ------
#        $boolean = $dtr->notify($pdr)
#
#        $boolean = isaSuccess($pdr)
#        $boolean = isComplete($pdr)
#
#        $dtr = DTR->new($file,%config)
#        $pathname = $dtr->genPDR()
#        $pathname = $dtr->exportPDR($pdr_name)
#        @pdr = $dtr->getPDR()
#        $disposition = $dtr->valPDR($PDRname)
#        $dtr_new = $dtr->resolve($PDRname)
#        @objects = $dtr->getProd($PDRname);
#        @objects = $dtr->valProd($PDRname);
#        @objects = $dtr->getObjects(@file);
#        @files = $dtr->files($pdr);
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# getPDR()            function      PUB  returns a list of PDR pathnames
#                                        by polling a server for new PDRs.
#
# genPDR()              method      PUB  generates a PDR based on the data
#                                        transaction parameters describing the
#                                        PDR and associated products. Each
#                                        product is returned as a reference to
#                                        a HASH table containing parameters
#                                        defined by the PDR file specification
#                                        objects.
#
# getProd()             method      PUB  acquires the data products described
#                                        in the specified PDR. Each product
#                                        is returned as a reference to
#                                        a HASH table containing parameters
#                                        defined by the PDR file specification
#                                        objects.
#
# valPDR()              method      PUB  validates the specified PDR by
#                                        comparing the parameters defined within
#                                        the PDR to the expected parameters
#                                        defined within the data transaction
#                                        file. Missing parameters or unexpected
#                                        values are recorded and returned as a
#                                        string describing the disposition. The
#                                        default disposition is "SUCCESS".
#
# valProd()             method      PUB  validates the specified products by
#                                        deriving file related parameters from
#                                        the acquired data and comparing the
#                                        calculated values with the values
#                                        contained within the PDR. Discrepencies
#                                        are recorded and returned as a string
#                                        describing the disposition. The default
#                                        disposition is "SUCCESS".
#
# notify()              method      PUB  notifies the server by issuing a
#                                        product delivery record discrepency
#                                        (PDRD) or a product acceptance notice
#                                        (PAN) based on the dispositions 
#                                        resulting from valPDR() and valProd()
#                                        respectively. 
#
# isaSuccess            method      PUB  
#
# $dtr                referent      OUT  HASH referent returned by DTR->new()
#
# $fname                string       IN  pathname of data transaction file.
#
# %config                 HASH   OPT,IN  Hash table of parameter definitions
#                                        for resolving variable names.
#
# $pdr                  string       IN  pathname of a PDR file.
#
# @pdr                  string      OUT  list of PDR pathnames.
#
# @product                HASH    INOUT  array of hash references. There is
#                                        a hash reference for each file
#                                        specification object block within
#                                        a PDR file. Each hash contains key/val
#                                        entries for each parameter defined in
#                                        the object block.
#
# $disposition          string      OUT  Outcome of validation. "SUCCESS" is
#                                        returned when the validation encounters
#                                        no errors.
#
# $boolean             boolean      OUT  "1" or "0" indicating true or false
#                                        respectively.
#                                        
#                               
#                                      
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#
#           07/06/2013      J.Ardizzone  created.
#******************************************************************************
use strict;

use File::Basename;
use File::Path;
use File::Spec;
use File::Copy;
use Net::SSH qw(ssh issh sshopen2 sshopen3);
use Net::SCP;
use Digest::MD5;
use Fcntl qw( :DEFAULT );

use DTR::PDR;
use DTR::PAN;
use DTR::PDRD;
use DTR::OBJECT;
use CONFIG;

use CLOCK;
use Time::Piece;
use Time::Local;
use Time::Seconds;

our %LISTING = ();
our %PAN_TIMES = ();

#******************************************************************************
sub new { my $invocant = shift;
          my %config   = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Module Constructor
# -------------
#
# Purpose: This is a generic constructor for instantiating transaction
# -------- objects. It is inherited by transaction modules.
#
# Language: Perl
# ---------
#
# Notes: 1. Transactions are typically instantiated based on parameters
# ------    specified in a transaction (".dtr") file. The constructor for the
#           DTR.pm module implements a polymorphic mechanism by determining the
#           transaction protocol and instantiating objects from sub-classes of
#           this module.
#
# See Also: DTR::FTP.pm, DTR::SCP.pm, DTR::CP.pm, DTR::NULL.pm, DTR.pm
# ---------
#
# Usage: $dtr = DTR->new($file,%config)
# ------ 
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                     DTR      OUT  Object referent blessed as one of the
#                                        sub-class types: DTR::FTP, DTR::SCP,
#                                        DTR::CP, DTR::NULL.
#
# $file                 string       IN  Filename of a data transaction file
#                                        containing parameters that determine
#                                        the transaction type.
#
# %config                 HASH   OPT,IN  Hash of pre-defined parameters.
#                                        Supplied values are returned with the
#                                        blessed hash referent.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           06/30/2013      J.Ardizzone  created.
#******************************************************************************

  my $class  = ref($invocant) || $invocant;
  my $self   = \%config;

  bless($self, $class);
}

#******************************************************************************
sub genPDR { my $self = shift;
#******************************************************************************
# English Name: Generate PDR
# -------------
#
# Purpose: Generates a PDR according to the parameter specifications
# -------- associated with the invoking object (see constructor for more
#          information).
#
# Language: Perl
# ---------
#
# Notes: 1. File size and checksum parameters are resolved using this method.
# ------
#
# See Also: DTR.pm, DTR::OBJECT.pm, DTR::PDR.pm
# ---------
#
# Prerequisites: 
# --------------
#
# Usage: $pathname = $dtr->genPDR()
# ------ 
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                invocant       IN  Invoking DTR hash object (see 
#                                        constructor).
#
# $pathname             string      OUT  name of the generated PDR file.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/23/2015      J.Ardizzone  created.
#******************************************************************************

  DTR_OBJECT: my @objects = $self->getObjects();

  $self->clear_cache();

  foreach my $obj (@objects) { $obj->resolve() }

  my $path = $self->{PDR_LOCAL_DIR};
  my $name = $self->{PDR_NAME}; 
  my $pathname = File::Spec->catfile($path, $name);

  my $pdr = DTR::PDR->new();
  $pdr->write($self,@objects);

  return $pathname;

}

#******************************************************************************
sub exportPDR { my $self = shift;
                my $pdr_name = shift;
#******************************************************************************
# English Name: Export PDR
# -------------
#
# Purpose: Generates an export PDR according to the parameter specifications
# -------- associated with the invoking object (see constructor for more
#          information).
#
# Language: Perl
# ---------
#
# Notes: 1. An export PDR is a copy of a PDR with file specifications that are
# ------    valid from a client perspective (i.e. the source directory contains
#           the login path on the server).
#
# See Also: DTR.pm, DTR::OBJECT.pm, DTR::PDR.pm, CONFIG.pm
# ---------
#
# Prerequisites:
# --------------
#
# Usage: $pathname = $dtr->exportPDR($pdr_name)
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                invocant       IN  Invoking DTR hash object (see
#                                        constructor).
#
# $pdr_name             string       IN  Filename of the exported PDR.
#
# $pathname             string      OUT  name of exported PDR file (default:
#                                        $pdr_name if $dtr does not contain
#                                        export parameters).
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           01/23/2015      J.Ardizzone  created.
#******************************************************************************

  $self->clear_cache();

# Return the same name if the
# export directory is unspecified.
# ================================

  $self->{PDR_EXPORT_DIR} or return $pdr_name;

# Retrieve the product specification
# objects from the DTR and PDR files.
# ===================================

  CONFIG: my $cfg = CONFIG->new();
  my @pdr = $cfg->configure($pdr_name);

  DTR_OBJECT: my @dtr_object = $self->getObjects();
  DTR_OBJECT: my @pdr_object = $self->getObjects(@pdr);

# For a PDR to be exported, the source
# directory (DIRECTORY_ID) becomes the
# local export directory specified by
# FILE_LOCAL_DIR (unless overridden 
# with an explicit declaration of the
# client directory).
# ====================================

  foreach my $index (0..$#dtr_object) {

    my $dtr_obj = $dtr_object[$index];
    my $pdr_obj = $pdr_object[$index];

    $dtr_obj->{FILE_LOCAL_DIR} or next;

    $pdr_obj->{FILE_ID} = $dtr_obj->{FILE_LOCAL_NAME} // $dtr_obj->{FILE_ID};

    $pdr_obj->{DIRECTORY_ID} = $dtr_obj->{FILE_CLIENT_DIR} // 
                                      $dtr_obj->{FILE_LOCAL_DIR};

  }

# Create the export PDR
# =====================

  my $path = $self->{PDR_EXPORT_DIR};
  my $name = basename $pdr_name;
  my $pathname = File::Spec->catfile($path, $name);

  DTR_PDR: my $pdr = DTR::PDR->new();

  $pdr->write({%$self, PDR_LOCAL_DIR=>$path,
                       PDR_NAME=>$name, 
                       DTR_FILE=>\@pdr},
                       @pdr_object);

  return $pathname;

}

#******************************************************************************
sub getPDR { my $self = shift;
#******************************************************************************
# English Name: Get PDR
# -------------
#
# Purpose: Retrieves PDRs from a remote server.
# -------- 
#
# Language: Perl
# ---------
#
# Notes: 1. This method will revisit PDRs that remain on the server. This is
# ------    known as PDR recovery. These PDRs will be re-executed if the
#           current disposition indicates failure.
#
# See Also: DTR.pm
# ---------
#
# Prerequisites:
# --------------
#
# Usage: @pdr = $dtr->getPDR()
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                invocant       IN  Invoking DTR hash object (see
#                                        constructor).
#
# @pdr                   ARRAY      OUT  Local filenames of the acquired PDRs.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           01/23/2015      J.Ardizzone  created.
#******************************************************************************

  my $name      = $self->{PDR_NAME};
  my $path      = $self->{PDR_REMOTE_DIR};
  my $pathname  = File::Spec->catfile($path,$name);
  my $local_dir = $self->{PDR_LOCAL_DIR};

# Return if the location of the
# remote PDRs is not specified.
# =============================

  if (! $path) { return () }

# Retrieve a listing of the 
# remove files.
# =========================

  my @file = $self->list($pathname);

# Form a list of local files and
# a remote listing of new files.
# ===============================

  my @PDRlocal  = ();
  my @PDRremote = ();

  foreach my $file (@file) {

    $name = basename $file;
    $path = $local_dir;
    $pathname = File::Spec->catdir($path,$name);

    if (! -s $pathname) { push @PDRremote, $file }

    push @PDRlocal, $pathname;

  }

# Retrieve the PDRs from the remote server.
# Return a listing of all PDRs (old and new).
# This enables failed PDRs to be revisited and
# re-executed if necessary.
# ============================================

  mkpath $local_dir;
  if (@PDRremote) { $self->get($local_dir,@PDRremote) }

  return @PDRlocal;
}

#******************************************************************************
sub valPDR { my $self    = shift;
             my $PDRname = shift;
#******************************************************************************
# English Name: Validate PDR
# -------------
#
# Purpose: Determines if the supplied PDR parameters match the specifications
# -------- of the data transaction object. The disposition of the PDR check
#          is returned as a string.
#
# Language: Perl
# ---------
#
# Notes: 1. The returned disposition will be "SUCCESS" if the PDR file matches
# ------    the expected parameters defined by the associated transaction. Any
#           value other than success indicates a mismatch.
#
#        2. Only PDR parameters are compared. Parameters within the extended
#           DTR namespace are not compared.
#
# See Also: DTR.pm, PDRD.pm, CONFIG.pm, OBJECT.pm
# ---------
#
# Prerequisites:
# --------------
#
# Usage: $disposition = $dtr->valPDR($PDRname)
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                invocant       IN  Invoking DTR hash object (see
#                                        constructor).
#
# $PDRname              string       IN  Filename of PDR to be validated.
#
# $disposition          string      OUT  Disposition of the PDR validation using
#                                        the namespace reserved for PDR
#                                        discrepancies (see PDRD.pm). "SUCCESS"
#                                        is returned for successful validation.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           01/23/2015      J.Ardizzone  created.
#******************************************************************************

  $self->clear_cache();

# Construct the pathname of the
# PDRD file.
# =============================

  my $name = basename $PDRname;
  my $PDRDname = File::Spec->catdir($self->{PDRD_LOCAL_DIR}, $name);
  $PDRDname =~ s/\.PDR$/\.PDRD/i;

# Check for zero-length or missing PDR file.
# ==========================================

  my $disposition = "MISSING PARAMETER";
  my $pdrd = DTR::PDRD->new();

  -s $PDRname or do {
    $pdrd->write($PDRDname,$disposition);
    return $disposition;
  };

# Read in the PDR file and extract
# the file specification objects.
# ================================

  my $cfg = CONFIG->new();
  my @pdr = $cfg->configure($PDRname);
  my @PDRobjects = $self->getObjects(@pdr);

# Retrieve the file specification
# objects from the data transaction file
# (self-contained with the invoking object).
# ==========================================

  my @DTRobjects = $self->getObjects();

# Validate the PDR
# ================

  foreach my $index (0..$#DTRobjects) {

    my $DTRobj = $DTRobjects[$index];
    my $PDRobj = $PDRobjects[$index];

    if ($DTRobj eq $PDRobj) {$disposition = "SUCCESS"; next}

    $disposition = $DTRobj->{DISPOSITION};

    last;
  }

  $disposition eq "SUCCESS" or $pdrd->write($PDRDname,$disposition);
  return $disposition;
}

#******************************************************************************
sub resolve { my $self = shift;
              my $PDRname = shift;
#******************************************************************************
# English Name: Resolve
# -------------
#
# Purpose: Instantiates a new transaction (DTR) object by resolving the
# -------- transaction file associated with the incovant with the date and time
#          derived from the specified PDR file. This is a recursive
#          instantiation that yields a post factum object resolved with the date
#          and time extracted from the PDR file content.
#
# Language: Perl
# ---------
#
# Notes: 1. The CLOCK_METHOD parameter must be specified by the invocant. Clock
# ------    methods are able to determine the date/time from the products
#           contained within the PDR. Clock methods can vary. A passive clock
#           simply extracts the date/time from filenames defined in the PDR.
#           Active clocks may use more intrusive methods that involve reading
#           files defined in the PDR. See CLOCK.pm for more information.
#
# See Also: DTR.pm, CLOCK.pm
# ---------
#
# Prerequisites:
# --------------
#
# Usage: $dtr_new = $dtr->resolve($PDRname)
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                invocant       IN  Invoking DTR hash object (see
#                                        constructor).
#
# $PDRname              string       IN  Filename of PDR.
#
# $dtr_new                 DTR      OUT  Data transaction object resolved with
#                                        date/time of the specified PDR file.
#                                        The invoking object is returned if 
#                                        the CLOCK_METHOD parameter is not
#                                        specified (i.e. $dtr is returned as
#                                        $dtr_new).
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           01/23/2015      J.Ardizzone  created.
#******************************************************************************

  $self->{CLOCK_METHOD} or return $self;

  my $t  = CLOCK->new();
  my $method = $self->{CLOCK_METHOD};
  $t = $t->$method($PDRname,DATETIME=>1);

  $t or return $self;

  my $date = $t->strftime("%Y%m%d");
  my $time = $t->strftime("%H%M%S");

  return DTR->new($self->{DTR_FILE_ID},%$self,DATE=>$date,TIME=>$time);

}

#******************************************************************************
sub getProd { my $self = shift;
              my $PDRname = shift;
#******************************************************************************
# English Name: Get Products
# -------------
#
# Purpose: Executes a data transaction by retrieving or referencing the files
# -------- described in the supplied PDR file.
#
# Language: Perl
# ---------
#
# See Also: DTR.pm, DTR->new(), OBJECT.pm
# ---------
#
# Usage: @objects = $dtr->getProd($PDRname);
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                 DTR ref       IN  Reference to an object with the DTR
#                                        class.
#
# $PDRname              string       IN  Filename of the PDR.
#
# @objects            ARRAY of      OUT  Array of references to objects with
#                   OBJECT ref           the OBJECT class. Each object
#                                        describes a file that was retrieved
#                                        or referenced (passive retrieval).
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           08/18/2014      J.Ardizzone  documented.
#******************************************************************************

# Read the PDR file
# =================

  my $cfg = CONFIG->new();
  my @pdr = $cfg->configure($PDRname);

  @pdr or return ();

# Extract the objects from the PDR
# file and the corresponding DTR file
# ===================================

  my @DTRobjects = $self->getObjects();
  my @PDRobjects = $self->getObjects(@pdr);

# Execute the transaction
# =======================

  foreach my $index (0..$#DTRobjects) {

    my $DTRobj = $DTRobjects[$index];
    my $PDRobj = $PDRobjects[$index];

    my $name     = $PDRobj->{FILE_ID};
    my $path     = $PDRobj->{DIRECTORY_ID};
    my $pathname = File::Spec->catdir($path, $name);

#   Transaction type-1: file retrieval
#   ==================================

    if ($DTRobj->{FILE_LOCAL_DIR} ) {

      my $DTRroot = $self->{DTR_DIR_ID};
      my $DTRpath = $DTRobj->{FILE_LOCAL_DIR};

      File::Spec->file_name_is_absolute($DTRpath) or do
             { $DTRpath = File::Spec->catdir($DTRroot,$DTRpath) };

      $DTRobj->{FILE_LOCAL_DIR} = $DTRpath;

#     Retrieve/copy the file
#     ----------------------

      mkpath $DTRobj->{FILE_LOCAL_DIR};
      $self->get($DTRobj->{FILE_LOCAL_DIR},$pathname);
  
      $DTRobj->{FILE_ID}      = $name;
      $DTRobj->{DIRECTORY_ID} = $DTRobj->{FILE_LOCAL_DIR};

    }

#   Transaction type-2: file reference
#   ==================================

    else { 

      $DTRobj->{FILE_ID}        = $name;
      $DTRobj->{FILE_LOCAL_DIR} = $PDRobj->{DIRECTORY_ID};

    }

#   Resolve the dynamic parameters
#   such as filesize and checksum.
#   ==============================

    $DTRobj->resolve();

  }

  return @DTRobjects;
}

#******************************************************************************
sub genProd { my $self = shift;
#******************************************************************************
# English Name: Generate Products
# -------------
#
# Purpose: Generates products from templates described in the data transaction
# -------- objects.
#
# Language: Perl
# ---------
#
# See Also: DTR.pm, DTR->new(), OBJECT.pm
# ---------
#
# Notes: 1. A templated object is a DTR object with the TEMPLATE_ID parameter
# ------    specified. The referenced template file typically contains XML
#           data describing the data file referenced in the preceding DTR
#           object. The preceding object is known as the target.
#
# Usage: $dtr->genProd();
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                 DTR ref       IN  Reference to an object with the DTR
#                                        class.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           05/16/2016      J.Ardizzone  documented.
#******************************************************************************

  my $date = $self->{DATE};
  my $time = $self->{TIME};

# Look for template objects and execute
# the generation of the product file
# =====================================

  my @objects = $self->getObjects();

  foreach my $index (1..$#objects) {

    my $object = $objects[$index];
    my $target = $objects[$index-1];

    if (exists $object->{TEMPLATE_ID}) {

#     Derive template parameters
#     --------------------------

      my $granule_id = File::Spec->catfile($target->{DIRECTORY_ID},
                                           $target->{FILE_ID});

      $object->{LocalGranuleID} = $target->{FILE_LOCAL_NAME} //
                                  $target->{FILE_ID};

      my $ctime    = (stat($granule_id))[9];
      my $t_create = gmtime($ctime);

      $object->{ProductionDateTime} =
        $object->{ProductionDateTime} //
        $t_create->strftime("%Y-%m-%d %H:%M:%S.000");

      exists $object->{RANGE_BEGINNING} and do {
        my $t_beg  = CLOCK->new(DATE=>$date, TIME=>$time);
        $t_beg    += $object->{RANGE_BEGINNING};
        $object->{RangeBeginningDate} = $t_beg->strftime("%Y-%m-%d");
        $object->{RangeBeginningTime} = $t_beg->strftime("%H:%M:%S.000000");
      };

      exists $object->{RANGE_ENDING} and do {
        my $t_end  = CLOCK->new(DATE=>$date, TIME=>$time);
        $t_end    += $object->{RANGE_ENDING};
        $object->{RangeEndingDate} = $t_end->strftime("%Y-%m-%d");
        $object->{RangeEndingTime} = $t_end->strftime("%H:%M:%S.999999");
      };

#     Create a product file from the template
#     ---------------------------------------

      mkpath $object->{DIRECTORY_ID};
      my $in_file  = $object->{TEMPLATE_ID};
      my $out_file = File::Spec->catfile($object->{DIRECTORY_ID},
                                         $object->{FILE_ID});

      my ($fh_in, $fh_out);
      open $fh_in, "<$in_file";
      open $fh_out, ">$out_file";

      my $config = CONFIG->copy(%$self,%$object);
      $config->sed($fh_in,$fh_out,DELIMITER=>'@');
      close $fh_in; close $fh_out;

    }

  }

  return;
}

#******************************************************************************
sub valProd { my $self = shift;
              my $PDRname = shift;
              my @DTRobjects = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Validate Products
# -------------
#
# Purpose:
# -------- 
#
# Language: Perl
# ---------
#
# See Also: DTR.pm, DTR->new(), OBJECT.pm
# ---------
#
# Usage: @objects = $dtr->valProd($PDRname);
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                 DTR ref       IN  Reference to an object with the DTR
#                                        class.
#
# $PDRname              string       IN  Filename of the PDR.
#
# @objects            ARRAY of    INOUT  Array of references to objects with
#                   OBJECT ref           the OBJECT class. Each object
#                                        describes a file that was retrieved
#                                        or referenced (passive retrieval).
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           08/18/2014      J.Ardizzone  documented.
#******************************************************************************


  my $cfg;
  my ($atime, $mtime);
  my $t = CLOCK->new();
  my ($DTRobj, $PDRobj, @pdr, @PDRobjects);
  my ($name, $PANname, $pan, $disposition);

  $self->clear_cache();

  $pan = DTR::PAN->new();
  $disposition = "SUCCESS";

  $cfg = CONFIG->new();
  @pdr = $cfg->configure($PDRname);
  @PDRobjects = $self->getObjects(@pdr);

  $DTRobj = $DTRobjects[0];

  $name = basename $PDRname;
  $PANname = File::Spec->catdir($self->{PAN_LOCAL_DIR}, $name);
  $PANname =~ s/\.PDR$/\.PAN/i;

  foreach $DTRobj (@DTRobjects) {

    $PDRobj = shift @PDRobjects;
    $PDRobj->{DIRECTORY_ID} = $DTRobj->{FILE_LOCAL_DIR};

    if ($DTRobj eq $PDRobj) {next}

    $disposition = $DTRobj->{DISPOSITION};
    last;
  }

  $atime = $t->epoch;
  $mtime = $atime;
  $mtime = (stat($PANname))[9] if -s $PANname;

  $pan->write($PANname,$disposition);

  utime $atime, $mtime, $PANname;

  return $disposition;
}

sub moveProd {

  my $self = shift;
  my @DTRobjects = @_;

  my $DTRobj;
  my ($localDir, $inFile, $outFile);

  foreach $DTRobj (@DTRobjects) {

    $DTRobj->{FILE_LOCAL_NAME} or next;

    $localDir = $DTRobj->{FILE_LOCAL_DIR};

    $inFile =  File::Spec->catfile($localDir, $DTRobj->{FILE_ID});
    $outFile = File::Spec->catfile($localDir, $DTRobj->{FILE_LOCAL_NAME});

    next if ! -e $inFile;

    rename $inFile, $outFile or print "moveProd: $!\n";

  }

}

sub notify
{

  my $self = shift;
  my $PDRname = shift;

  my $dtr;
  my ($name, $PDRDname, $PANname);

  $name = basename $PDRname;

  $PDRDname = File::Spec->catdir($self->{PDRD_LOCAL_DIR}, $name);
  $PDRDname =~ s/\.PDR$/\.PDRD/i;

  if ( -s $PDRDname ) {

    $dtr = DTR::getProtocol($self->{PDRD_REMOTE_LOGIN});
    $dtr->put($self->{PDRD_REMOTE_DIR}, $PDRDname);

    return 1;

  }

  $PANname = File::Spec->catdir($self->{PAN_LOCAL_DIR}, $name);
  $PANname =~ s/\.PDR$/\.PAN/i;

  if ($self->isComplete($PDRname)) {

    $dtr = DTR::getProtocol($self->{PAN_REMOTE_LOGIN});
    $dtr->put($self->{PAN_REMOTE_DIR}, $PANname);

    return 1;

  }

  return 0;

}

sub isaSuccess
{

  my $self = shift;
  my $PDRname = scalar(@_) ? shift : undef;

  my ($pan, @pan);
  my ($name, $PANname);

  $name = File::Spec->catdir($self->{PDR_LOCAL_DIR},$self->{PDR_NAME});
  $PDRname = $PDRname // $name;

  $name = basename $PDRname;

  $PANname = File::Spec->catdir($self->{PAN_LOCAL_DIR}, $name);
  $PANname =~ s/\.PDR$/\.PAN/i;

  if ( ! -s $PANname) { return 0 }

  $pan = CONFIG->new();
  @pan = $pan->configure($PANname);

  if ($pan->{DISPOSITION} =~ /^SUCCESSFUL$/i) { return 1 }

  return 0;
}

sub isComplete
{

  DTR:    my $self     = shift;
  STRING: my @pathname = scalar(@_) ? @_ : ();

  STRING: my $pathname;

  foreach $pathname (@pathname) { 

    $self->isaSuccess($pathname) && next;
    $self->isWaiting($pathname) && return 0;
    
  }

  return 1;
}

sub isWaiting
{

# Argument List
# =============

  DTR:    my $self     = shift;
  STRING: my @pathname = scalar(@_) ? @_ : ();

# Local Variables
# ===============

  STRING:  my ($name, $pathname, $PANname);
  INTEGER: my ($atime, $mtime, $ctime);

  $self->{WAIT_TIME} = $self->{WAIT_TIME} // 0;
# $self->{WAIT_TIME} or return 0;
# @pathname or return 1;

  foreach $pathname (@pathname) {

    $self->isaSuccess($pathname) && next;

    $name     = basename $pathname;
    $PANname = File::Spec->catdir($self->{PAN_LOCAL_DIR}, $name);

    $PANname =~ s/\.PDR$/\.PAN/i;

    (-s $PANname) or return 1;

    ($atime, $mtime, $ctime) = (stat($PANname))[8,9,10];
    ($atime - $mtime) < $self->{WAIT_TIME} && return 1;

  }

  return 0;
}

sub isaExport
{
  my $self = shift;
  return 1 if exists ($self->{PDR_EXPORT_DIR}) ;
  return 0;
}

sub isaImport
{
  my $self = shift;
  return 0 if exists ($self->{PDR_REMOTE_DIR}); 
  return 1;
}

sub isaFailure
{
  my $self = shift;
  my @object = @_;

  foreach (@object) { if ($_->{DISPOSITION} ne "SUCCESS") {return 0 } }
}

sub isScheduled
{
    my $self = shift;
    $self->{SCHEDULE} or return 1;

    my @dates = split ' ', $self->{SCHEDULE};
    foreach my $date (@dates) { return 1 if $self->{DATE} == $date }

    return 0;
}

#******************************************************************************
sub getObjects { my $self = shift;
                 my @file = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Get Objects
# -------------
#
# Purpose: Parses and extracts information for each object contained in the
# -------- supplied PDR file. The extracted objects are returned. See note-1.
#
# Language: Perl
# ---------
#
# See Also: DTR.pm, DTR::OBJECT.pm, CONFIG.pm, CONFIG::configure()
# ---------
#
# Notes: 1. PDR objects are extracted from the data transaction (DTR) file 
# ------    referenced by this object when the PDR file argument is unspecified.
#
# Usage: @objects = $dtr->getObjects(@file);
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                 DTR ref       IN  Reference to an object with the DTR
#                                        class.
#
# @file               ARRAY of   IN,OPT  Optional PDR file (stored as an array).
#                     strings            Objects are extracted from the data
#                                        transaction file referenced by this
#                                        object if the file argument is
#                                        omitted (see note-1).
#
# @objects            ARRAY of      OUT  Array of references to objects with
#                     OBJECT ref         the OBJECT class. Each object
#                                        describes a file/granule with all
#                                        associated parameters defined in the
#                                        object block.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           08/20/2014      J.Ardizzone  documented.
#******************************************************************************

# Use the self-described DTR file
# when the PDR file is unspecified.
# =================================

  my $file = $self->{DTR_FILE};
  @file = @$file if ! @file;

# Extract each object block from
# the file.
# ==============================

  my @objects;
  my $obj = DTR::OBJECT->new($self);

  foreach my $line (@file) {

    $line =~ s/ //g;
    my ($key, $value) = split '=', $line;
    $obj->{$key} = $value;

    if ($obj->{END_OBJECT} eq "FILE_SPEC") 
    {push @objects, $obj; $obj = DTR::OBJECT->new($self) }

    if ($obj->{OBJECT} eq "FILE_SPEC") { next } 
    else { $obj = DTR::OBJECT->new($self) }

  }

  return @objects;
}

sub search
{

  my $self = shift;
  my %options = scalar(@_) ? @_ : ();

  my @pdr = ();
  my $pathname = File::Spec->catdir($self->{PDR_LOCAL_DIR},"*");

  $self->{SEARCH_RESULT} = 1;

  SWITCH: foreach (keys %options) {

    /^CLOCK$/ and do {
      @pdr = $self->search_by_clock($options{CLOCK});
      return @pdr;
    };

    /^PAN_DATE$/ and do {
      @pdr = $self->search_by_pan_date($options{PAN_DATE});
      return @pdr;
    };

    /^DATE$/ and do {
      @pdr = $self->search_by_date($options{DATE},ALL=>$options{ALL});
      return @pdr;
    };

    /^PATTERN$/ and do {
      @pdr = $self->search_by_pattern($options{PATTERN});
      return @pdr;
    };

  }

  @pdr = $self->search_by_transaction(%options);
# @pdr = < $pathname >;

  return @pdr;
  
}

#******************************************************************************
sub files { my $self = shift;
            my $pdr  = ref($_[0]) ? undef : shift;
            my $options  = ref($_[0]) ? shift : {};
#******************************************************************************
# English Name: Files
# -------------
#
# Purpose: Returns a list of all files/granules described in the specified
# -------- PDR file. See notes-1 and 2.
#
# Language: Perl
# ---------
#
# See Also: DTR.pm, DTR::OBJECT.pm, CONFIG.pm, CLOCK.pm
# ---------
#
# Notes: 1. A filelist is extracted from the data transaction (DTR) file
# ------    referenced by this object when the PDR filename argument is
#           unspecified. The returned filenames will not be resolved in time.
#           This is a convenient way to retrieve filename templates.
#
#        2. A PDR describes the source granules. This function returns the
#           local granule names as described in the DTR file referenced by this
#           object. The local and source filenames will be identical for
#           transactions that simply reference local granules (i.e. no transfer
#           or copy of the granules).
#
# Usage: @files = $dtr->files($pdr);
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $dtr                 DTR ref       IN  Reference to an object with the DTR
#                                        class.
#
# $pdr                  string   IN,OPT  Optional PDR filename. The PDR 
#                                        described granule names will be
#                                        extracted from the transaction file
#                                        referenced by this object if the
#                                        filename argument is omitted (see 
#                                        note-1).
#
# @files              ARRAY of      OUT  Array of filenames described in the
#                     filenames          specified PDR file. See note-1.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           08/21/2014     J. Ardizzone  documented.
#
#******************************************************************************

# Retrieve the date/time from
# the PDR file using the associated
# clock method.
# =================================

  my $clock  = CLOCK->new();
  my $method = $self->{CLOCK_METHOD} // "timeless";
  my $time   = $clock->$method($pdr,DATETIME=>1) if $pdr;

# Retrieve the PDR objects that describe
# the granules to be listed. The PDR file
# is set to the DTR file when unspecified.
# ========================================

  $pdr = $pdr // $self->{DTR_FILE_ID};

  my $config   = CONFIG->new();
  my @dtr_file = $config->configure($self->{DTR_FILE_ID},{%$self, TIMELESS=>1});
  my @pdr_file = $config->configure($pdr,{%$self, TIMELESS=>1});

  my @dtr_object = $self->getObjects(@dtr_file);
  my @pdr_object = $self->getObjects(@pdr_file);

# Create the list of granules described
# in the PDR. Information from the DTR
# file is used to construct the local
# filenames.
# ========================================

  my @files = ();

  foreach my $index (0..$#dtr_object) {

    my $dtr_obj = $dtr_object[$index];
    my $pdr_obj = $pdr_object[$index];

    $dtr_obj->{FILE_TYPE} eq $options->{FILE_TYPE} 
                           or next if $options->{FILE_TYPE};

    $dtr_obj->{FILE_LOCAL_DIR} or next if $options->{LOCAL};

    my $path  = $dtr_obj->{FILE_LOCAL_DIR} // $pdr_obj->{DIRECTORY_ID};
    $path     = $pdr_obj->{DIRECTORY_ID} if $options->{SOURCE};
    $path     = $time->strftime($path) if $time;

    my $name  = $dtr_obj->{FILE_LOCAL_NAME} // $pdr_obj->{FILE_ID};
    $name     = $pdr_obj->{FILE_ID} if $options->{SOURCE};
    $name     = $time->strftime($name) if $time;

    my $pathname = File::Spec->catdir($path,$name);

    if (! File::Spec->file_name_is_absolute($path) )
        { $pathname = File::Spec->catfile($self->{DTR_DIR_ID},$pathname) }

    push @files, $pathname;

  }

  return @files;
}

sub search_by_pattern
{

  my $self = shift;
  my $pattern = shift;

  my $file;
  my @pdr = ();
  my $pathname = File::Spec->catdir($self->{PDR_LOCAL_DIR},"*");
  my $files    = $self->get_listing($pathname);

  foreach $file (@$files) {

    open FILE, "<$file";

    while (<FILE>) {

      if ( $_ =~ /$pattern/ ) {
        push @pdr, $file;
        last;
      }

    }

    close FILE;

  }

  return @pdr;

}

sub search_by_transaction
{
  REFERENT: my $self = shift;
  my %options = scalar(@_) ? @_ : ();

  INTEGER: my $failure = 0;
  STRING:  my @pdr = ();
  STRING:  my $method = $self->{CLOCK_METHOD};

  $self->{SEARCH_RESULT} = 1;
  my ($tstart, $tend, $inc_sec) = $self->times();

  CLOCK: my $t = $tstart;

  my $day       = $tstart->strftime("%Y%m%d");
  my $end_day   = $tend->strftime("%Y%m%d");

  while ($day <= $end_day) {

    my $date = $t->strftime("%Y%m%d");
    my $time = $t->strftime("%H%M%S");

    my $dtr = DTR->new($self->{DTR_FILE_ID}, %$self, DATE=>$date,TIME=>0);

    my @file = $dtr->search(DATE=>$dtr->{DATE},ALL=>1);

    $failure = 1 if @file == 0;

#   Limit the search to only those
#   files within the transaction time
#   window.
#   =================================

    foreach my $file (@file) {

      CLOCK: my $tfile = $t->$method($file,DATETIME=>1);

      $tfile and do { $tfile < $tstart and next;
                      $tfile > $tend and next;
                    };

      $self->isaSuccess($file) or do { $failure = 1; next if ! $options{ALL} };

      push @pdr, $file;
    }

    $t += 86400;
    $day = $t->strftime("%Y%m%d");

  }

  $self->{SEARCH_RESULT} = 0 if $failure;

  return @pdr;

}

sub search_by_date
{
  my $self = shift;
  my $date = scalar(@_) ? shift : $self->{DATE};
  my %options = scalar(@_) ? @_ : ();

  my $file;
  my @pdr = ();
  my $clock = CLOCK->new(DATE=>$date,TIME=>0);
  my $method = $self->{CLOCK_METHOD};
  my $pathname = File::Spec->catdir($self->{PDR_LOCAL_DIR},"*");
  my $files = $self->get_listing($pathname);

  foreach $file (@$files) { 

    $clock->$method($file)   || next;
    $self->isaSuccess($file) || next if ! $options{ALL};

    push @pdr, $file;

  }

  return @pdr;
    
}

sub get_listing
{
  my $self = shift;
  my $glob = shift;

  $LISTING{$glob} or do {
    my @listing = < $glob >;
    $LISTING{$glob} = \@listing;
  };

  return $LISTING{$glob};
}

sub clear_cache { my $self = shift; %LISTING = (); %PAN_TIMES = () }

sub search_by_pan_date
{
  my $self = shift;
  my $date = scalar(@_) ? shift : $self->{PAN_DATE};

  my @pdr = ();

  my $path     = $self->{PAN_EXPORT_DIR} // $self->{PAN_LOCAL_DIR};
  my $pathname = File::Spec->catfile($path,"*");
  my $listing  = $self->get_listing($pathname);

  foreach my $pan (@$listing) {

    my $ctime = $PAN_TIMES{$pan} // (stat($pan))[9];
    $PAN_TIMES{$pan} = $ctime;

    my $time  = gmtime($ctime);
    my $pan_date = $time->strftime("%Y%m%d");

    next if $pan_date ne $date;

    my $name = basename $pan; $name =~ s/\.PAN/\.PDR/;
    my $pdr  = File::Spec->catfile($self->{PDR_LOCAL_DIR}, $name);

    -s $pdr or next;

    push @pdr, $pdr;

  }

  return @pdr;

}

sub search_by_clock
{
  my $self = shift;
  my $time = shift;

  my $file;
  my @pdr = ();
  my $method = $self->{CLOCK_METHOD};
  my $date = $time->strftime("%Y%m%d");
  my @file = $self->search_by_date($date);

  foreach $file (@file) {

    $time->$method($file,DATETIME=>1) == $time  || next;
    $self->isaSuccess($file) || next;

    push @pdr, $file;

  }

  return @pdr;
}

#sub find { REFERENT: $self = shift;
#           CLOCK:    $time = shift;

#  my $dtr = DTR->new($self->{DTR_FILE_ID},TIMELESS=>1);
#  my $pdr = $time->strftime($dtr->{PDR_PATHNAME

sub times
{
  REFERENT: my $self = shift;

  INTEGER: my $date       = $self->{DATE};
  INTEGER: my $start_time = $self->{START_TIME} // 0;
  INTEGER: my $end_time   = $self->{END_TIME}  // 0;
  INTEGER: my $inc_sec    = $self->{INC_SEC} // 3600;
  INTEGER: my ($sdate, $stime, $edate, $etime);

# Set the starting and ending times.
# ==================================

  my $tstart = CLOCK->new(DATE=>$date,TIME=>0);
  my $tend   = CLOCK->new(DATE=>$date,TIME=>0);

  $tstart += $start_time;
  $tend += $end_time;

  $sdate = $tstart->strftime("%Y%m%d");
  $stime = $tstart->strftime("%H%M%S");
  $edate = $tend->strftime("%Y%m%d");
  $etime = $tend->strftime("%H%M%S");

  return ($tstart, $tend, $inc_sec, $sdate, $stime, $edate, $etime);

}

sub checksum { my $self     = shift;
               my $pathname = shift;

  my $fh;
  open $fh, "<$pathname";
  my $md5 = Digest::MD5->new();
  $md5->addfile($fh);
  close $fh;

  return $md5->hexdigest();

}

sub filesize { my $self     = shift;
               my $pathname = shift;

  return -s $pathname;
}


1;
