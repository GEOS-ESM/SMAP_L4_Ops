package L4_SM::OBSLOG;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use CLOCK;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Package Constructor
# -------------
#
# Purpose: Instantiates a Level-4 Soil Moisture Observation Log object.
# -------- A hash table referent is returned containing a dictionary of all
#          parameters defined within the file. 
#
# Language: Perl
# ---------
#
# Usage: $obslog = L4_SM::OBSLOG->new($file,$time)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $obslog             referent      OUT  hash referent for instantiated OBSLOG
#                                        object. 
#
# $filename             string       IN  OBSLOG filename.
#
# $time                  CLOCK       IN  analysis time. Observation info
#                                        is returned for this time only.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           04/08/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  INVOCANT: my $invocant = shift;
  STRING:   my $filename = shift;
  CLOCK:    my $time = scalar(@_) ? shift : undef;

# Local Variables
# ---------------

  REFERENT: my $self = {};
  my $class = ref($invocant) || $invocant;

  $self->{ERROR_HANDLER} = "error_handler";

  bless($self, $class);

  -f $filename or return $self;
  $time or return $self;

  $self->read($filename,$time);

  return $self;
}

#******************************************************************************
sub read()
#******************************************************************************
# English Name: Read
# -------------
#
# Purpose: Reads in parameters from a Level-4 Soil Moisture Observation Log
# -------- (OBSLOG) file.
#
# Language: Perl
# ---------
#
# Usage: $obslog->read($filename,$time);
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $obslog             referent    INOUT  hash referent for instantiated OBSLOG
#                                        object. Parameters are returned as
#                                        hash entries. Parameters with multiple
#                                        attributes are returned as pointers
#                                        to a hash of the attributes and values.
#
# $filename             string       IN  OBSLOG filename.
#
# $time                  CLOCK       IN  analysis time. Observation info
#                                        is returned for this time only.
# 
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/21/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING:   my $filename = shift;
  CLOCK:    my $time = shift;

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;
  ARRAY_REF:   my $array;
  HASH_REF:    my $hash;
  ANY:         my ($key, $value);
  STRING:      my $date_time = $time->strftime("%Y%m%d_%H%M%S") if $time;

  open $fh, "<$filename";

  while (<$fh>) {

    chomp;

    /^(\d{8}_\d{6})z,\s*(\w+),\s*(\w+).*,\s+(\w+),\s*(.*)/ and do {

      next if $date_time ne $1 and $time;

      $key  = $2;
      $hash = $self->{$key} // {};
      $hash->{FILENAMES}  = $hash->{FILENAMES}  // [];
      $hash->{SUBROUTINE} = $hash->{SUBROUTINE} // [];
      $hash->{OBS_COUNT}  = $hash->{OBS_COUNT}  // [];

      $array = $hash->{SUBROUTINE};
      push @$array, $3;

      $array = $hash->{OBS_COUNT};
      push @$array, $4;

      $array = $hash->{FILENAMES};
      push @$array, $5;

      $self->{$key}  = $hash;
      next;

    };

  }

  close $fh;

  return;
}

1;
