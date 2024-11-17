package L4_SM::QA;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use Time::Seconds;
use Time::Piece;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Package Constructor
# -------------
#
# Purpose: Instantiates a Level-4 Soil Moisture Quality Assurance (QA) object.
# -------- A hash table referent is returned containing a dictionary of all
#          parameters defined within the file. 
#
# Language: Perl
# ---------
#
# Usage: $qa = L4_SM::QA->new($file)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $qa                 referent      OUT  hash referent for instantiated QA
#                                        object. 
#
# $filename             string       IN  QA filename.
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
# Purpose: Reads in parameters from a Level-4 Soil Moisture Quality Assurance
# -------- (QA) file.
#
# Language: Perl
# ---------
#
# Usage: $qa->read($filename);
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $qa                 referent    INOUT  hash referent for instantiated QA
#                                        object. Parameters are returned as
#                                        hash entries. Parameters with multiple
#                                        attributes are returned as pointers
#                                        to a hash of the attributes and values.
#
# $filename             string       IN  QA filename.
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

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;
  HASH_REF:    my $hash;
  ANY:         my ($key, $value);

  FILE_CREATION: my $ctime = (stat($filename))[10];
  FILE_CREATION: my $t = gmtime($ctime);

  $self->{QAFileName} = basename $filename;
  $self->{QACreationDate} = $t->datetime . ".000Z";

  open $fh, "<$filename";

  while (<$fh>) {

    chomp;

    /^\s*([\w,\s]+)\s+=\s*(.*)/ and do {

      $key = $1;
      $value = "$2";
      $value =~ s/;$//;
      $self->{$key} = $value;

      next;

    };

    /^\s*(\w+)\s*,\s*(.*),\s*(.*),\s*(.*),\s*(.*),\s*(.*),\s*(.*)/ and do {

      $key  = $1;
      $hash = {};
      $hash->{UNITS} = $2;
      $hash->{MEAN}  = $3;
      $hash->{STD}   = $4;
      $hash->{MIN}   = $5;
      $hash->{MAX}   = $6;
      $hash->{COUNT} = $7;

      $self->{$key}  = $hash;
      next;

    };

  }

  close $fh;

  return;
}

1;
