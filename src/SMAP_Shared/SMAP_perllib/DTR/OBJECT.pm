                        #######################
                        #                     #
                        # PACKAGE DTR::OBJECT #
                        #                     #
                        #######################


#******************************************************************************
package DTR::OBJECT;
#******************************************************************************
# English Name: Data Transaction Object Package
# -------------
#
# Purpose: Provides methods for extracting and managing file specification
# -------- objects from PDR files.
#
# Language: Perl
# ---------
#
# Usage: $obj = DTR::OBJECT->new()
# ------ $obj->resolve()
#        $obj->print()
#
#        $obj->is_equal($obj2) ? 1 : 0
#        ($obj eq $obj2) ? 1 : 0
#
#        $obj->is_not_equal($obj2) ? 1 : 0
#        ($obj ne $obj2) ? 1 : 0
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# new()            DTR::OBJECT      PUB  Creates a HASH referent for managing
#                                        PDR file specification constructs
#                                        following the object data language
#                                        (ODL) syntax.
#
# resolve()            boolean      PUB  Resolves undefined parameters in a
#                                        PDR file specification object by
#                                        deriving the missing fields (e.g.
#                                        filesize, checksum).
#
# is_equal()           boolean      PUB  Comparator for determining if two 
#                                        objects are equivalent. Overloads
#                                        the "eq" operator. 
#
# is_not_equal()       boolean      PUB  Comparator for determining if two 
#                                        objects are equivalent. Overloads
#                                        the "ne" operator. 
#
# print()              boolean      PUB  prints the HASH referent key/value
#                                        pairs.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/16/2013      J.Ardizzone  created.
#******************************************************************************
use strict;

use DTR::NULL;

use overload "eq" => \&is_equal,
             "ne" => \&is_not_equal;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Constructor for Object Package
# -------------
#
# Purpose: Creates a Perl object for managing PDR file specification 
# -------- constructs following the object data language (ODL) syntax.
#
# Language: Perl
# ---------
#
# Usage: $obj = DTR::OBJECT->new()
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $obj             DTR::OBJECT      OUT  function return value: empty HASH
#                                        referent.
#                                        
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/15/2013      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  my $invocant = shift;
  my $handle   = scalar(@_) ? shift : DTR::NULL->new();
  my $class = ref($invocant) || $invocant;

  my $obj = {FILE_EXIST=>0, HANDLE=>$handle};

  bless ($obj, $class);
}

#******************************************************************************
sub resolve
#******************************************************************************
# English Name: Resolve
# -------------
#
# Purpose: Resolves undefined parameters in a PDR file specification object
# -------- by deriving the missing fields (e.g. filesize, checksum).
#
# Language: Perl
# ---------
#
# Usage: $object->resolve()
# ------
#
# Notes: 1. This method recognizes the following PDR parameters:
# ------
#           FILE_SIZE (bytes)
#           FILE_CKSUM_VALUE (MD5 hexidecimal)
#
#           All other parameters will remain unaffected. Values are only
#           supplied if the recognized parameters are undefined.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $object          DTR::OBJECT       IN  HASH referent. On input, hash table
#                                        of PDR file specification parameters
#                                        with possible undefined values. On
#                                        output, hash table with undefined
#                                        parameters resolved (see note-1).
#                                       
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/15/2013      J.Ardizzone  created.
#******************************************************************************
{
  my $object = shift;
  my $handle = $object->{HANDLE};

# Derive the pathname to the
# data file.
# ==========================

  my $name  = $object->{FILE_ID};
  my $path  = $object->{DIRECTORY_ID};
  my $pathname = File::Spec->catdir($path, $name);

# Derive and assign values to
# undefined parameters.
# ============================

  foreach (keys %$object) {

    /^FILE_EXIST$/         &&  do { 
                                    $object->{FILE_EXIST} = 1;
                                    next if -f $pathname;
                                    $object->{FILE_EXIST} = 2;
                                  };

    /^FILE_SIZE$/          &&  do {
                                    next if $object->{FILE_SIZE};
                                    $object->{FILE_SIZE} = 0;
#                                   next unless -f $pathname;

#                                   $object->{FILE_SIZE} = (-s $pathname);
                                    my $filesize = $handle->filesize($pathname);

                                    $object->{FILE_SIZE} = $filesize;
                                    next;
                                   };

    /^FILE_CKSUM_VALUE$/   &&  do {
                                    next if $object->{FILE_CKSUM_VALUE};
                                    $object->{FILE_CKSUM_VALUE} = 0;
                                    next unless -f $pathname;

#                                   my $fh;
#                                   open $fh, "<$pathname";
#                                   my $md5 = Digest::MD5->new();
#                                   $md5->addfile($fh);
#                                   close $fh;

#                                   my $chksum = $md5->hexdigest();
                                    my $chksum = $handle->checksum($pathname);

                                    $object->{FILE_CKSUM_VALUE} = $chksum;
                                    next;
                                  };
  }

  return 1;
}

#******************************************************************************
sub print
#******************************************************************************
{
  my $object = shift;
  foreach my $key (keys %$object) { print "$key = $object->{$key}\n" }
  return 1;
}

#******************************************************************************
sub is_equal
#******************************************************************************
# English Name: Is It Equal?
# -------------
#
# Purpose: Comparator for determining if two objects are equivalent.
# --------
#
# Language: Perl
# ---------
#
# Usage: $rc = ($DTRobj eq $PDRobj)
# ------
#
# See Also: is_not_equal(): overloads "ne" operator.
# ---------
#
# Notes: 1. Only valid PDR file specification parameters are compared. All
# ------    other parameters are ignored.
#
#        2. The left operand is returned with the {DISPOSITION} key set to 
#           the result of the comparison. The value is relevant to the PAN
#           and PDRD transaction disposition values and is used to create
#           these notification files. "SUCCESS" is returned when the two
#           objects are equal.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $DTRobj           DTR::OBJECT   INOUT  HASH referent. On input, the left
#                                        operand is typically the data
#                                        transaction template defining the
#                                        PDR parameters that must be defined
#                                        in the right operand. On output,
#                                        the HASH key {DISPOSITION} is set
#                                        (see note-2).
#
# $PDRobj           DTR::OBJECT      IN  HASH referent. The right operand must
#                                        define the parameters contained in the
#                                        left operand. If the left operand
#                                        parameter is defined, then the values
#                                        of the left and right parameters must
#                                        be equal.
#
# $rc                   boolean     OUT  function return value:
#
#                                        0: the two operands are not equal
#                                        1: the two operands are equal
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/15/2013      J.Ardizzone  created.
#******************************************************************************
{

  my $DTRobj = shift;
  my $PDRobj = shift;

  my $DTRval;
  my $PDRval;
  my $pdr = DTR::PDR->new();

  SWITCH: foreach (keys %$DTRobj) {

    $DTRval = $DTRobj->{$_};
    $PDRval = $PDRobj->{$_};
    $DTRobj->{DISPOSITION} = "SUCCESS";

    # MISSING PARAMETER

    exists $pdr->{$_}     && do {
                               if ( ! defined $PDRval) {
                                 $DTRobj->{DISPOSITION} = "MISSING PARAMETER";
                                 return 0;
                               }
                             };

    # NULL PARAMETER

                             if (! $DTRval) {next};

    /^FILE_EXIST$/        && do {
                               next if $DTRval == 1;
                               $DTRobj->{DISPOSITION} = "TRANSFER FAILURE";
                               return 0;
                             };

    /^FILE_ID$/           && do {
                               if ( $PDRval =~ /$DTRval/ ) {next} 
                               $DTRobj->{DISPOSITION} = "FILE_ID DISCREPENCY";
                               return 0;
                             };

    /^DIRECTORY_ID$/      && do {
                               if ( $PDRval =~ /$DTRval/ ) {next} 
                               $DTRobj->{DISPOSITION} = 
                                         "DIRECTORY_ID DISCREPENCY";
                               return 0;
                             };

    /^FILE_SIZE$/         && do {
                               if ( $PDRval == $DTRval ) {next} 
                               $DTRobj->{DISPOSITION} = "FILE SIZE FAILURE";
                               return 0;
                             };

    /^FILE_CKSUM_VALUE$/  &&  do {
                               if ( $PDRval eq $DTRval ) {next} 
                               $DTRobj->{DISPOSITION} = "CHECKSUM FAILURE";
                               return 0;
                             };

    # DEFAULT

                             if (exists $pdr->{$_} && $DTRval ne $PDRval) {
                               $DTRobj->{DISPOSITION} = "UNEXPECTED VALUE";
                               return 0;
                             };

  }

  return 1;
}

#******************************************************************************
sub is_not_equal
#******************************************************************************
{

  my $obj1 = shift;
  my $obj2 = shift;

  return (! is_equal($obj1,$obj2));

}

1;
