
                        #######################
                        #                     #
                        #  PACKAGE DTR::PDR   #
                        #                     #
                        #######################

#******************************************************************************
package DTR::PDR;
#******************************************************************************
# English Name: Data Transaction PDR Package
# -------------
#
# Purpose: Provides a constructor for creating a dictionary of valid PDR
# -------- parameters.
#
# Language: Perl
# ---------
#
# Usage: $pdr = DTR::PDR->new()
# ------ $pdr_copy = $pdr->new()
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# new()               DTR::PDR      PUB  Creates a HASH referent containing
#                                        keys that map directly to valid PDR
#                                        parameter names. The returned HASH
#                                        can be used to determine if a named
#                                        parameter is part of the PDR language.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           07/15/2013      J.Ardizzone  created.
#******************************************************************************

use strict;

use File::Path;
use File::Spec;

sub new
{

# Argument List
# -------------

  my $invocant = shift;
  my $class = ref($invocant) || $invocant;

  my $pdr = {};

  $pdr->{ORIGINATING_SYSTEM} = 1;
  $pdr->{TOTAL_FILE_COUNT}   = 1;
  $pdr->{EXPIRATION_TIME}    = 1;
  $pdr->{OBJECT}             = 1;
  $pdr->{DATA_TYPE}          = 1;
  $pdr->{DATA_VERSION}       = 1;
  $pdr->{NODE_NAME}          = 1;
  $pdr->{DIRECTORY_ID}       = 1;
  $pdr->{FILE_ID}            = 1;
  $pdr->{FILE_TYPE}          = 1;
  $pdr->{FILE_SIZE}          = 1;
  $pdr->{FILE_CKSUM_TYPE}    = 1;
  $pdr->{FILE_CKSUM_VALUE}   = 1;
  $pdr->{END_OBJECT}         = 1;

  bless($pdr, $class);
}

sub write
{
  my $pdr  = shift;
  my $dtr  = shift;
  my @objects = scalar(@_) ? @_ : ();

  my %type;
  my $i = 0;
  my $ntab = 0;
  my $indent;
  my ($line, $key, $value);
  my $name = $dtr->{PDR_NAME};
  my $path = $dtr->{PDR_LOCAL_DIR};
  my $pathname = File::Spec->catdir($path, $name);
  my $file = $dtr->{DTR_FILE};
  my @file = @$file;

  if (! -d $path) {mkpath $path or die "Unable to create PDR directory\n"}
  open WRITER, ">$pathname" or die "Unable to write PDR\n";

  foreach $line (@file) {

    $line =~ s/ //g;
    ($key, $value) = split '=', $line;
    if (! exists $pdr->{$key}) { next }

    $type{$key} = $value;
    if ($type{OBJECT} eq "FILE_SPEC") {$value = $objects[$i]->{$key} }

    if ($key eq "END_OBJECT" && $value eq "FILE_SPEC") { 
      delete $type{OBJECT};
      $i++ if $i < $#objects;
    }

    if ($key eq "END_OBJECT") { --$ntab }

    $indent = "   " x $ntab;
    print WRITER "$indent$key = $value;\n";

    if ($key eq "OBJECT") { $ntab++ }

  }

  close WRITER;

  return 1;

}

1;
