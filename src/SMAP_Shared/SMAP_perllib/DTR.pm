
package DTR;

# Use "perldoc DTR" to retrieve documentation for this module.
#******************************************************************************

=head1 NAME

DTR - Data Transaction

=head1 DESCRIPTION

Container class for instantiating data transaction (DTR) objects. This
package provides the constructor and logic for interpreting data
transaction files (.dtr) and returning a hash referent. The referent
class is determined at run-time based on the supplied information in the
transaction file.

=head1 SYNOPSIS

$dtr = DTR->new($fname,%config);

=over 14

=item new()

Constructor for instantiating a data transaction object. Returned objects
are derived from sub-classes of DTR::TRANSACTION. Available types are: DTR::FTP, DTR::SCP, DTR::CP, DTR::DMF and DTR::NULL.

=item $dtr

Data transaction object. The object class is determined using information
from the data transaction file (see note-2).

=item $fname

Filename of the data transaction file (see note-1).

=item %config

Any key/value pairs comprising an application specific dictionary. Parameters
within the dictionary are used to interpolate variables (if any) in the data
transaction file (see note-3).

=item

On output, the dictionary is augmented with parameters defined within the DTR
file and additional parameters required for implementation (see note-4).

=back

=head1 SEE ALSO

DTR::TRANSACTION, DTR::SCP, DTR::FTP, DTR::CP, DTR::DMF, DTR::NULL, CONFIG.pm

=head1 NOTES

=over 7

=item 1.

Data transaction files are comprised of parameter definitions following the Object Data Language (ODL) syntax and extending the conventions used for Product Description Records (PDRs).

=item 2.

The returned referent implements the API defined by the transaction protocol modules (e.g. DTR::SCP) and inherits the methods of the parent class, DTR::TRANSACTION.

=item 3.

Variable and time interpolation functionality are provided by the "CONFIG" module. Variable names are enclosed in angle brackets (e.g. <USER_NAME>) and are resolved using the supplied dictionary (see %config argument). Time variable tokens follow the Unix 'date' command conventions (e.g. %Y - four digit year). The current date/time is used unless the following parameters are defined on input:

 DATE - date as %Y%m%d (e.g. 20130401 for April 1, 2013)
 TIME - time as %H%M%S (e.g. 230000 for 23Z)

=item 4.

The returned hash referent will contain the following parameters.  These parameters will overwrite any parameters with the same name that are specified as part of the input configuration. Please use alternate names to avoid collisions with the DTR namespace.

 DTR_FILE
 DTR_FILE_ID
 DTR_DIR_ID
 PDR_REMOTE_LOGIN
 PAN_REMOTE_LOGIN
 PDRD_REMOTE_LOGIN
 PDR_REMOTE_DIR
 PAN_REMOTE_DIR
 PDRD_REMOTE_DIR
 PDR_LOCAL_DIR
 PAN_LOCAL_DIR
 PDRD_LOCAL_DIR
 CLOCK_METHOD
 PDR_PATHNAME

=back

=head1 AUTHORS

Joseph V. Ardizzone

=head1 COPYRIGHT

This software is the property of the National Aeronautics and Space
Administration (NASA) and is subject to the regulations contained in the NASA
Procedural Requirements document NPR 2210.1C managed by the Office of the Chief
Technologist.

=cut

#******************************************************************************
# English Name: Data Transaction (DTR)
# -------------
#
# Purpose: Container class for instantiating data transaction objects. This 
# -------- package provides the constructor and logic for interpreting data
#          transaction (.dtr) files and returning a hash referent. The referent
#          is run-time polymorphically determined based on the supplied
#          information in the transaction file (see note-2).
#
# Language: Perl
# ---------
#
# See Also: DTR::TRANSACTION, DTR::SCP, DTR::FTP, DTR::CP, DTR::NULL, CONFIG.pm
# ---------
#
# Usage: $dtr = DTR->new($fname,%config)
# ------
#
# Notes: 1. Data transaction files are comprised of parameter definitions
# ------    following the Object Data Language (ODL) syntax and extending the
#           conventions used for Product Description Records (PDRs).
#
#        2. The returned referent implements the API defined by the transaction
#           protocol modules (e.g. DTR::SCP) and inherits the methods of the
#           parent class, DTR::TRANSACTION.
#
#        3. Variable and time interpolation functionality are provided by the
#           "CONFIG" module. Variable names are enclosed in angle brackets
#           (e.g. <USER_NAME>) and are resolved using the supplied dictionary
#           (see %config argument). Time variable tokens follow the Unix 'date'
#           command conventions (e.g. %Y - four digit year). The current
#           date/time is used unless the following parameters are defined on
#           input:
#
#           DATE - date as %Y%m%d (e.g. 20130401 for April 1, 2013)
#           TIME - time as %H%M%S (e.g. 230000 for 23Z)
#
#        4. The returned hash referent will contain the following parameters.
#           These parameters will overwrite any parameters with the same name
#           that are specified as part of the input configuration. Please
#           use alternate names to avoid collisions with the DTR namespace.
#
#           DTR_FILE
#           DTR_FILE_ID
#           DTR_DIR_ID
#           PDR_REMOTE_LOGIN
#           PAN_REMOTE_LOGIN
#           PDRD_REMOTE_LOGIN
#           PDR_REMOTE_DIR
#           PAN_REMOTE_DIR
#           PDRD_REMOTE_DIR
#           PDR_LOCAL_DIR
#           PAN_LOCAL_DIR
#           PDRD_LOCAL_DIR
#           CLOCK_METHOD
#           PDR_PATHNAME
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $fname                string       IN  pathname of the data transaction 
#                                        file (.dtr). See note-1.
#
# %config                 hash    INOUT  Any key/value pairs 
#                                        comprising an application specific
#                                        dictionary. Parameters within the
#                                        dictionary are used to interpolate
#                                        variables (if any) in the DTR file
#                                        (see note-3).
#
#                                        On output, the dictionary is augmented
#                                        with parameters defined within the 
#                                        DTR file and additional parameters
#                                        required for implementation 
#                                        (see note-4).
#
# $dtr           hash referent      OUT  Perl object implementing the API
#                                        defined for data transactions (see
#                                        note-2 and 4).
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#
#           07/06/2013      J.Ardizzone  created.
#           09/05/2013      J.Ardizzone  documented.
#******************************************************************************

use strict;
use warnings;

use File::Basename;
use File::Path;
use File::Spec;

use DTR::CP;
use DTR::SCP;
use DTR::FTP;
use DTR::NULL;
use DTR::DMF;
use DTR::DMF::Handler;
use CONFIG;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Data Transaction (DTR) Constructor
# -------------
#
# Purpose: Creates an object for executing data transactions based on a
# -------- specified transaction file (.dtr) and optional configuration
#          parameters for resolving variables within the file.
#
# Language: Perl
# ---------
#
# Usage: $dtr = DTR->new($fname,%config)
# ------
#
# Notes: 1. This is a polymorphic container method for instatiating an object.
# ------    The appropriate object class is determined based on the defined
#           transaction protocol.
#
#        2. This constructor is a container method for creating referents of
#           type "DTR::TRANSACTION". Therefore, this method is never invoked
#           as an instance. 
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# fname                 string       IN  pathname of the data transaction 
#                                        file (.dtr).
#
# config                  hash   OPT,IN  Hash table of parameter definitions
#                                        for resolving variable names.
#
# dtr            hash referent      OUT  function return value. The class
#                                        type will be a subclass of 
#                                        DTR::TRANSACTION as determined by
#                                        this method.
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
{

# Argument List
# -------------

  STRING: my $invocant = shift;
  STRING: my $fname    = shift;
  HASH:   my %config   = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  CONFIG: my $cfg;
  STRING: my @file;

# Resolve the data transaction
# (DTR) file using the supplied
# configuration parameters.
# ============================

  $config{DTR_FILE_ID} = $fname;
  $config{DTR_DIR_ID}  = dirname $fname;

  $cfg = CONFIG->new();
  @file = $cfg->configure($fname,\%config);

# Set DTR parameters and set
# defaults for undefined parameters
# =================================

  $config{DTR_FILE} = \@file;	
  $config{DTR_FILE_ID} = $fname;
  $config{DTR_DIR_ID}  = dirname $fname;

  DTR::setDefaults(\%config);

# Return a DTR-type object referent 
# based on the defined transaction
# protocol type.
# =================================

  return DTR::getProtocol($config{PDR_REMOTE_LOGIN},%config);
}

#******************************************************************************
sub setDefaults
#******************************************************************************
# English Name: Set Defaults
# -------------
#
# Purpose: Sets key data transaction parameters to default values (if not
# -------- defined) and initializes the data transaction environment. 
#
# Language: Perl
# ---------
#
# Usage: $rc = DTR::setDefaults(\%config)
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# config                  hash    INOUT  On input: Hash table of parameter 
#                                        definitions. On output, hash table
#                                        with missing key parameters set to 
#                                        default values. 
#
# rc                   boolean      OUT  function return value:
#
#                                        0: exception thrown
#                                        1: success
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/06/2013      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  my $config = shift;

# Local Variables
# ---------------

  my $DTRdir = $config->{DTR_DIR_ID};

# Set Default parameters
# ======================

  if (! $config->{PDR_REMOTE_LOGIN})  {$config->{PDR_REMOTE_LOGIN} = "CP://"   }

  if (! $config->{PAN_REMOTE_DIR})    {$config->{PAN_REMOTE_LOGIN} = "NULL://" }

  if (! $config->{PAN_REMOTE_LOGIN})  {$config->{PAN_REMOTE_LOGIN} = "CP://"   }

  if (! $config->{PDRD_REMOTE_DIR})   {$config->{PDRD_REMOTE_LOGIN} = "NULL://"}

  if (! $config->{PDRD_REMOTE_LOGIN}) {$config->{PDRD_REMOTE_LOGIN} = "CP://"  }

  if (! $config->{PDR_LOCAL_DIR})     {$config->{PDR_LOCAL_DIR} =
                                          File::Spec->catdir($DTRdir,"PDR")    }

  if (! $config->{PAN_LOCAL_DIR})     {$config->{PAN_LOCAL_DIR} =
                                          File::Spec->catdir($DTRdir,"PAN")    }

  if (! $config->{PDRD_LOCAL_DIR})    {$config->{PDRD_LOCAL_DIR} =
                                          File::Spec->catdir($DTRdir,"PDRD")   }

  if (! $config->{CLOCK_METHOD})      {$config->{CLOCK_METHOD} = "timeless"    }

  $config->{PDR_PATHNAME} = File::Spec->catdir($config->{PDR_LOCAL_DIR},
                                               $config->{PDR_NAME});

  return 1;

}

#******************************************************************************
sub getProtocol
#******************************************************************************
# English Name: Get Protocol
# -------------
#
# Purpose: Interprets the supplied "login" string and returns an object
# -------- referent based on the encoded sequence: "protocol://user@machine" .
#          The returned referent implements the data transaction API (see
#          DTR::TRANSACTION).
#
# Language: Perl
# ---------
#
# Usage: $ref = DTR::getProtocol($login,%config)
# ------
#
# Notes: 1. The returned referent implements the data transaction API (see
# ------    DTR::TRANSACTION).
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# login                 string       IN  encoded string describing the
#                                        protocol and login information for
#                                        acquiring remote data. This method
#                                        currently recognizes the following
#                                        protocols:
#
#                                        SCP://  - Secure Copy
#                                        FTP://  - File Transfer Protocol
#                                        CP://   - Unix Copy
#                                        DMF://  - Data Migration Facility
#                                        NULL:// - undefined (deactivated)
#
# config                  hash    INOUT  Any contents on input are returned
#                                        as part of the hash referent. Two
#                                        additional parameters are added to
#                                        describe the user and machine
#                                        information extracted from the login
#                                        string:
#
#                                        {DTR_USER}    - login name
#                                        {DTR_MACHINE} - machine name
#
# ref                      ref      OUT  returned hash referent associated with
#                                        the specified protocol.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           09/04/2013      J.Ardizzone  documented.
#******************************************************************************
{

# Argument List
# -------------

  my $login = shift;
  my %config = scalar(@_) ? @_ : ();

# Local Variables
# ===============

  my @login;
  my ($user, $machine, $protocol);

# Parse the login string for protocol,
# user and machine information.
# ====================================

  @login      = split /:/, $login;
  ($protocol) = split /:/, $config{DTR_PROTOCOL} // $login[0];

  @login    = split /@/, $login[1];
  $user     = substr($login[0],2);
  $machine  = $login[1];

  $config{DTR_USER} = $user;
  $config{DTR_MACHINE} = $machine;

# Return a referent implementing the
# API for the supplied protocol.
# DEFAULT: CP (unix copy).
# ==================================

  SWITCH: foreach ($protocol) {

    /^CP$/i      &&  do { return DTR::CP->new(%config) };
    /^SCP$/i     &&  do { return DTR::SCP->new(%config) };
    /^FTP$/i     &&  do { return DTR::FTP->new(%config) };
    /^DMF$/i     &&  do { return DTR::DMF->new(%config) };
    /^NULL$/i    &&  do { return DTR::NULL->new(%config) };

    return DTR::CP->new(%config);

  }

  return DTR::CP->new(%config);

}

1;
