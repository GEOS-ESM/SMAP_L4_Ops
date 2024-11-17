package L4_SM::LOG;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use Time::Piece;
use Time::Local;
use METADATA;

#******************************************************************************
sub lfo
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
  HASH:     my %options = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  REFERENT: my $self = \%options;
  my $class = ref($invocant) || $invocant;

  bless($self, $class);

  -f $filename or return $self;

  $self->readLFO($filename);

  return $self;
}

#******************************************************************************
sub readLFO()
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
  STRING:      my $missing = "missing";
  HASH_REF:    my ($hash1, $hash2, $hash3, $hash4);
  ARRAY_REF:   my ($array1, $array2, $array3, $array4);
  STRING:      my ($pathname,$name, $date_time);
  STRING:      my ($name, $creationDate, $doi, $version);
  METADATA:    my $meta = METADATA->new();

  open $fh, "<$file";

  $self->{LFO_fileName} = {};
  $self->{LFO_creationDate} = {};
  $self->{LFO_version} = {};
  $self->{LFO_DOI} = {};

  my $ctime = (stat($file))[10];
  my $t = gmtime($ctime);
  $self->{LOG_creationDate} = $t->datetime . ".000Z";

  while (<$fh>) {

    chomp;

    /^opening file:\s+(.*)/ and do {

      $pathname     = $1;

      $name         = basename $pathname;
      $creationDate = $meta->h5dump("-a ProductionDateTime " . $pathname);
      $doi          = $meta->h5dump("-a DOI " . $pathname);
      $version      = $meta->h5dump("-a VersionID " . $pathname);

      if ($creationDate) {

        $creationDate = (split /generated:/, $creationDate)[1];
        $t = Time::Piece->strptime($creationDate, " %a %b %e %H:%M:%S %Y GMT");
        $creationDate = $t->datetime . ".000Z";

      }

      $name =~ /(\d{8})_(\d{4})/;
      $date_time = $1 . $2;

      $hash1 = $self->{LFO_fileName};
      $hash1->{$date_time} = $hash1->{$date_time} // [];

      $hash2 = $self->{LFO_creationDate};
      $hash2->{$date_time} = $hash2->{$date_time} // [];

      $hash3 = $self->{LFO_version};
      $hash3->{$date_time} = $hash3->{$date_time} // [];

      $hash4 = $self->{LFO_DOI};
      $hash4->{$date_time} = $hash4->{$date_time} // [];

      $array1 = $hash1->{$date_time};
      $array2 = $hash2->{$date_time};
      $array3 = $hash3->{$date_time};
      $array4 = $hash4->{$date_time};

      grep /$name/, @$array1 or do {

        push @$array1, $pathname;
        push @$array2, $creationDate || $missing;
        push @$array3, $version || $missing;
        push @$array4, $doi || $missing;

      };

      next;

    };

  }

  close $fh;

  return;
}

1;
