package L4_SM::LOG;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use Time::Piece;
use Time::Local;

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
# Usage: $obslog = L4_SM::OBSLOG->new($file)
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
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/24/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  INVOCANT: my $invocant = shift;
  STRING:   my $filename = shift;

# Local Variables
# ---------------

  REFERENT: my $self = {};
  my $class = ref($invocant) || $invocant;

  $self->{ERROR_HANDLER} = "error_handler";

  bless($self, $class);

  -f $filename or return $self;

  $self->read($filename);

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
# Usage: $obslog->read($filename);
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
  STRING:   my $file = shift;

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;
  HASH_REF:    my $hash;
  ARRAY_REF:   my $array;
  STRING:      my $filename;
  INTEGER:     my $date_time = undef;
  INTEGER:     my ($year, $month, $day, $hour);
  INTEGER:     my $ctime = (stat($file))[10];
  TIME:        my $t = gmtime($ctime);

  open $fh, "<$file";

  $self->{LFO_fileName} = {};
  $self->{LFO_creationDate} = {};
  $self->{LOG_creationDate} = $t->datetime . ".000Z";

  while (<$fh>) {

    chomp;

    /^\s*START_TIME%YEAR\s*=\s*(\d+)/  and do { $year  = $1 };
    /^\s*START_TIME%MONTH\s*=\s*(\d+)/ and do { $month = sprintf("%01d",$1) };
    /^\s*START_TIME%DAY\s*=\s*(\d+)/   and do { $day   = sprintf("%01d",$1) }; 
    /^\s*START_TIME%HOUR\s*=\s*(\d+)/  and do { $hour  = sprintf("%01d",$1) };

    /^ date_time_new\s+(\d+)\.(\d+)\.(\d+)\.(\d+)/ and do {

      $date_time = $1 . $2 . $3 . $4;
      next;

    };

    /^read (SWGDN|PRECCU|HLML)\s+\w+\s+(.*)/ and do {

      $date_time = $date_time // $year . $month . $day . $hour;

      $filename = basename $2;
      $hash = $self->{LFO_fileName};
      $hash->{$date_time} = $hash->{$date_time} // [];

      $array = $hash->{$date_time};
      grep /$filename/, @$array or push @$array, $filename;

      next;

    };

  }

  close $fh;

  return;
}

1;
