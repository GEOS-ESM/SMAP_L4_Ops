#!/usr/bin/perl

BEGIN {

#  Keep track of errors within BEGIN block.

   $die_away = 0;

#  Load modules
#  ============

   use FindBin;
   use Net::FTP;
   use File::Path;
   use Getopt::Std;
   use Getopt::Long;
   use Sys::Hostname;
   use File::Basename;
   use Time::Piece;
   use Time::Local;
   use Time::Seconds;

#  Initialize options
#  ==================

   $opt_D = 0;
   $opt_E = 0;
   $opt_P = 0;
   $opt_O = 0;
   $opt_n = "mget_files";
   @opt_freq = ();

#  Get options and arguments
#  =========================

   GetOptions ('E=s',\$opt_E,
            'P=s',\$opt_P,
            'O=s',\$opt_O,
            't=s',\$opt_t,
	    'n=s',\$opt_n,
            'L=s',\$opt_L,
            'z',\$opt_z,
            'D',\$opt_D,
            'F',\$opt_F,
            'freq=i{1,2}',\@opt_freq,
            'sched_cnfg=s',\$sched_cnfg,
            'sched_id=s',\$sched_id,
            'sched_synp=s',\$sched_synp,
            'sched_c_dt=s',\$sched_c_dt,
            'sched_dir=s',\$sched_dir,
            'sched_sts_fl=s',\$sched_sts_fl,
            'sched_hs=s',\$sched_hs );
   
   $prep_ID = $ARGV[0];


#  OPTION -E: Prep_config file.
#  ----------------------------

   if ($opt_E) {
      $PREP_CONFIG_FILE = $opt_E;
   } else {
      $PREP_CONFIG_FILE = "DEFAULT";
   }

#  OPTION -P: Path to directory containing 
#  other GEOS DAS programs. Directory 
#  $GEOSDAS_PATH/bin will be searched for these programs.
#  ------------------------------------------------------

   if ($opt_P) { 
      $GEOSDAS_PATH = $opt_P;
   } else {
      $GEOSDAS_PATH = "DEFAULT";
   }

#  OPTION -t: The synoptic time to run.
#  (for flk mode only)
#  ------------------------------------

   if ( defined( $opt_t ) ) {
      $SYN_TIME = $opt_t;
      $syn_missing = 0;
   } else {
      $syn_missing = 1;
   }

#  ARGUMENT sched_id: If mget_files is initiated 
#  by the scheduler, construct table info. 
#  for "task_state" table of scheduler.
#  ------------------------------------------

   if ( defined( $sched_id ) )
   {
      $tab_status = 1;
      $tab_argv = "$sched_cnfg, $sched_id, $sched_synp, $sched_c_dt";
      $fl_name = "mget_files";
      $comd_wrt = "$sched_dir/utility/status";
      $args = "$fl_name COMPLETE $tab_argv $sched_dir";
   }

#  OPTION -L: Lag time for real-time processing 
#  (for llk mode only)
#  --------------------------------------------

   if ( defined( $opt_L ) ) {
     $LAG_TIME = $opt_L;
   } else {
     $LAG_TIME = 3;
   }

#  OPTION -O: Location for output listings
#  ---------------------------------------

   if ($opt_O) { 
     system ("mkdir -p $opt_O");
     if ( -w "$opt_O" ) {
       $listing_file   = "$opt_O/$opt_n.$$.listing";
       $listing_file_gz = "$opt_O/$opt_n.$$.listing.gz";

       print "Standard output redirecting to $listing_file\n";
       open (STDOUT, ">$listing_file");
       open (STDERR, ">&" . STDOUT);

     } else {
        print "$0: WARNING: $opt_O is not writable for listing.\n";
     }
   }
   else {
     $listing_file = "STDOUT";
   }

#  OPTION help: Set usage flag. flk 
#  and llk have different requirements; it 
#  is expected that llk will have no synoptic 
#  time in it's argument list.
#  ------------------------------------------

   $u_flag = 0;
   if ( $prep_ID eq 'flk') {
     if ( $#ARGV < 0 || $ARGV[0] eq 'help' || $syn_missing ) {
       $u_flag = 1;
     }
   } 
   else {
     if ( $#ARGV < 0 || $ARGV[0] eq 'help' ) {
       $u_flag = 1;
     }
   }

#  Set pathnames needed by this application
#  ========================================

#  If default GEOS DAS path, set path to 
#  parent directory of directory where this 
#  script resides.  
#  ----------------------------------------

   if ( $GEOSDAS_PATH eq "DEFAULT" ) {
     $GEOSDAS_PATH = dirname( $FindBin::Bin );
   }

#  Set name of the bin directory to search 
#  for other programs needed by this one.
#  ---------------------------------------

   $BIN_DIR = "$GEOSDAS_PATH/bin";
   $UTIL_DIR = "/usr/local/dasutils/bin";

#  Get the name of the directory where this
#  script resides.  If it is different than
#  BIN_DIR, then this directory will also be
#  included in the list of directories to 
#  search for modules and programs.
#  -----------------------------------------

   $PROGRAM_PATH = $FindBin::Bin;

#  Now allow use of any modules in the bin 
#  directory, and (if not the same) the
#  directory where this program resides.
#  (The search order is set so that the program's
#  directory is searched first, then the bin directory.)
#  -----------------------------------------------------

   if ( $PROGRAM_PATH ne $BIN_DIR ) {
     @SEARCH_PATH = ( $PROGRAM_PATH, $BIN_DIR, $UTIL_DIR );
   } else {
     @SEARCH_PATH = ( $BIN_DIR, $UTIL_DIR );
   }
} # End BEGIN

# Load local modules.
# ===================
  # Include the directories to be searched for required modules.
  use lib ( @SEARCH_PATH );

  # Set the path to be searched for required programs.
  $ENV{'PATH'} = join( ':', @SEARCH_PATH, $ENV{'PATH'} );


  use Err_Log;
  use Manipulate_time;
  use Extract_config;
  use Remote_utils;
  use Arch_utils;
  use Recd_State;

# End BEGIN: exit if errors
# =========================

  if ( $u_flag == 1 ) { printHelp(); $die_away = 1; }
  if ( $die_away == 1 ) { exit 1; }

# Include the directories to be searched for required modules.

  use lib ( @SEARCH_PATH );

# Set the path to be searched for required programs.

  $ENV{'PATH'} = join( ':', @SEARCH_PATH, $ENV{'PATH'} );

# Get the name of this (local) host, and then get the fully
# qualified local hostname (e.g., machine.gsfc.nasa.gov).
# =========================================================

  $host = hostname();
  print "host_name = $host \n";

# Set Event/Error log message prefix.
# ===================================

  if ( defined( $sched_id ) )  {
    $err_pref="$sched_id";
  }
  elsif ( ( ${opt_n} ) ) {
    $err_pref="mget_files_${opt_n}";
  }
  else {
    $err_pref="mget_files";
  }

# Write start message to Event Log
# ================================

  err_log (0, "mget_files.pl", "$prep_ID","$prep_ID","-1",
       {'err_desc' => "${err_pref}: job has started on $host" .
                " - Standard output redirecting to $listing_file"});

# Use Prep_Config file under the preprocessing 
# run's directory in the user's home directory
# as the default.
# ============================================

  if ( "$PREP_CONFIG_FILE" eq "DEFAULT" ) {
    $PREP_CONFIG_FILE = "$ENV{'HOME'}/$prep_ID/Prep_Config";
  }

  if ($opt_z) {
    print "Running with -z ... ignore 0-length files.\n";
  }

  if ($opt_F) {
    print "Running with -F ... ignore missing data.\n";
  }

# Dates are processed differently for flk
# and llk.  If date given, use that, otherwise
# use today's date (GMT).
# ============================================

  if ( $prep_ID eq 'flk' ) {

    if ( $#ARGV >= 1 ) {
      $process_date = date8( $ARGV[1] );

    } 

    else {

#     If we are processing 18Z, determine if we should subtract a day.

      ( $process_date, $current_time ) = z_time();
      if ( "${SYN_TIME}" == "18" && $current_time < 180000 ) {
        ( $process_date, $current_time ) = inc_time ($process_date, 
                                                $current_time, -1, 0);
      }
    }

  } 

  else {

    if ( $#ARGV >= 1 ) {
      $process_date = date8( $ARGV[1] );
    } 

    else {

#     Get current date (YYYYMMDD) in GMT, and set the process date to be
#     $LAG_TIME days prior.

      $process_date = ( z_time() )[0];
      ($process_date, $process_time) = 
                     inc_time ($process_date, 0, -$LAG_TIME, 0);
    }
}

# Generate timestamp for error messages.
# --------------------------------------

  if ( $syn_missing ) {
    $date_time_log = ${process_date};
  }
  else {
    $date_time_log = "${process_date}_${SYN_TIME}";
  }

# Get Data
# ========

# Extract list of file types.
# ===========================

  ( $FILE_TYPES = extract_config( "FILE_TYPES", 
                        $PREP_CONFIG_FILE, "NONE" ) ) ne "NONE" 
  or die "(mget_files) ERROR - cannot set FILE_TYPES configuration value\n";

# Transfer files for each data type
# or key.
# =================================

  foreach $key ( split(/,/, $FILE_TYPES) ) {

    print "Processing $key\n";

#   Retrieve configure and host information.
#   ----------------------------------------

    resolveConfig(\%config,$key) or die;
    getLocalHost(\%config,\%localHost) or die;
    getRemoteHost(\%config,\%remoteHost) or die;

#   Transfer files from the remote machine.
#   ---------------------------------------

    isaValidDay($process_date,@opt_freq) or next;

    @fileList = wgetFiles(\%remoteHost,\%localHost) if $remoteHost{url};
    @fileList = transferFiles(\%remoteHost,\%localHost) if $remoteHost{login};

    if (! @fileList && ! $opt_F) {
       recd_state( $fl_name, "FAILED", $tab_argv, $sched_dir, $sched_sts_fl );
       die;
    } 

#   Archive the files.
#   ------------------

    archiveFiles(\%config,@fileList) or die;

  }   # End foreach key.

  err_log (0, "mget_files.pl", "${date_time_log}","$prep_ID","-1",
       {'err_desc' => "${err_pref}: Running on $host - file" .
                    " file transfer complete."});

# Archive the listing file
# ========================

  archiveListing(\%config,$listing_file);

# Clean up and exit
# =================

  cleanup();

  exit 0;

#******************************************************************************
sub printHelp
#******************************************************************************
{
     print STDERR <<'ENDOFHELP';

Usage:

   mget_files.pl [-E Prep_Config] [-P GEOSDAS_Path] [-O output_location]
            [-n descriptive_job_name] [-t synoptic_time] [ -L lag_time] Prep_ID [process_date]

     Get_files.pl is a general PERL script to grab data files from a remote
     location.  Typically this location is an anonymous FTP server, although any
     protocol supported by the 'rget' utility of Remote_utils.pm can be used.
     The program reads an unlimited list of files from a Prep_Config file and
     places each file in its desired location.  Optional archiving to a secondary
     directory can be specified.  An example Prep_Config setup is shown:

        FILE_TYPES        = SBUV,TRMM

        FILE_SBUV_REMOTE  = anonymous@larry:/pub/data/OSBUV_%y2%m2/gdas1.%y2%m2%d2.t%h2z.osbuv.tm00.bufr_d
        FILE_SBUV_LOCAL   = /scratch1/lucchesi/stage_g5/sbuv/gdas1.%y2%m2%d2.t%h2z.osbuv.tm00.bufr_d
        FILE_SBUV_ARCHIVE = /output/lucchesi

        FILE_TRMM_REMOTE  = anonymous@larry:/pub/data/TRMM_%y2%m2/gdas1.%y2%m2%d2.t%h2z.sptrmm.tm00.bufr_d
        FILE_TRMM_LOCAL   = /scratch1/lucchesi/stage_g5/trmm/gdas1.%y2%m2%d2.t%h2z.sptrmm.tm00.bufr_d
        FILE_TRMM_ARCHIVE = /output/lucchesi

     FILE_TYPES is a common-separated list of file type tokens.  Each
     file type must have a FILE_type_REMOTE and FILE_type_LOCAL parameter
     specifying the remote location and local pathname of the file.  GrADS
     tokens can be used to represent dates and hours.  The FILE_type_ARCHIVE
     parameter is optional.  If present, data will be archived at the root directory
     specified. FILE_type_FILETYPE is another optional parameter for each file
     type.  If not present, "bufr" is assumed.  This is used in structuring the
     archive directories.

   Normal options and arguments:

   -E Prep_Config
         The full path to the preprocessing configuration file.  This file contains
         parameters needed by the preprocessing control programs. If not given, a
         file named $HOME/$prep_ID/Prep_Config is used.  mget_files exits with an
         error if neither of these files exist.

    -t synoptic_time
         The synoptic time to run.  This is an optional argument and will not
         typically be used for LLK processing.

    -n job name
         If multiple mget_files jobs are running, this can be useful for keeping listing
         files distinct.

    -L lag time
         This option is to be used in llk real time mode only.  This is the lag time, in
         days, before the current date.

    -freq [days]

    -freq [days] [dd] 

    -freq [days] [mmdd]

    -freq [days] [ccyymmdd]

         This options specifies the posting frequency of the desired data. The 
         second argument specifies the starting day (dd), month and day (mmdd)
         or the date (ccyymmdd). Month and day are set to 1 if not specified.
         The year is set to the year of the requested data if unspecified. The
         result is the reference date for beginning the stride. The default is
         January 1st in the year of the requested data.

    -z
         Force job to continue if some data sets are zero-length.  An error message will
         be generated, but the job will not die.

    -F
         Force job to continue if zero-length data sets are encountered or if any data sets are
         missing.  An error message will be generated, but the job will not die. 
    -D
         Use archive path as provided, do not expand path using gen_archive defaults.

   prep_ID
         Identification tag for this run.

   process_date
         Date in YYYYMMDD format to process.

   Options useful in development mode.  These are not necessary (and should not be
   used) when running this program in the usual operational environment.

   -P GEOSDAS_Path
         Path to directory containing other GEOS DAS programs.  The path is
         $GEOSDAS_PATH, where $GEOSDAS_PATH/bin is the directory containing these
         programs.  If -P GEOSDAS_Path is given, then other required programs not
         found in the directory where this program resides will be obtained from 
         subdirectories in GEOSDAS_Path - the subdirectory structure is assumed
         to be the same as the operational subdirectory structure.  The default is
         to use the path to the subdirectory containing this program, which is what
         should be used in the operational environment.

    -O output_location
         If this option is specified, output listings (both STDERR and STDOUT) will be
         placed in the directory "output_location."

ENDOFHELP

}

sub isaValidDay { my $date = shift;
                  my ($stride, $start_date) = @_;

  use integer;

  $stride > 0 or return 1;

  $stride *= 86400;
  $start_date = $start_date // 0;

  my $year  = $date / 10000;
  my $month = ($date - ($year * 10000) ) / 100;
  my $day   = $date - ($year * 10000) - ($month * 100);

  my $start_year  = $start_date / 10000;
  my $start_month = ($start_date - ($start_year * 10000) ) / 100;
  my $start_day   = $start_date - ($start_year * 10000) - ($start_month * 100);

  $start_day   = 1 if ! $start_day;
  $start_month = 1 if ! $start_month; 
  $start_year  = $year if ! $start_year;
  
  my $epoch       = timegm(0,0,0,$day,$month-1,$year);
  my $start_epoch = timegm(0,0,0,$start_day,$start_month-1,$start_year);

  return 0 if $epoch < $start_epoch;
  return ! (($epoch - $start_epoch) % $stride);

}

#******************************************************************************
sub isTransferred
#******************************************************************************
# English Name: Is The File Transferred?
# -------------
#
# Purpose: Checks for the existence of the specified file and reports files
# -------- with zero size.
#
# Language: Perl
# ---------
#
# Usage: isTransferred($filename)
# ------
#
# Notes: 1. Zero-size files will be ignored if the "-z" or "-F" command-line
# ------    options are specified.
#
#        2. This function heavily relies on global variables defined in the
#           main section of this application.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $filename             string       IN  local filename of the transferred
#                                        file.
#
# $isTransferred       boolean      OUT  function return value (see notes):
#
#                                        0: file is non-existent or zero size
#                                        1: file exists and has non-zero size
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/29/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my $file = shift;

# Return error if file is not found.
# ==================================

  if ( ! -f "$file" ) {

    recd_state($fl_name, FAILED, $tab_argv, $sched_dir, $sched_sts_fl);
    warn "(mget_files) ERROR: could not download $file\n";

    return 0;

  }

# No further checking is needed if file has
# non-zero size.
# =========================================

  if ( ! -z "$file" ) { return 1 };

# Report zero-size files and return if
# ignore options are set. Otherwise,
# issue a fatal error and return.
# ====================================

  if ( $opt_z || $opt_F ) {

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
        {'err_desc' => "${err_pref}: WARNING: $file is zero length"});

    print "WARNING: $file is zero length. " .
                 "Force flag in effect. Will continue.\n";

    return 1;
          
  }

  else {

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
        {'err_desc' => "${err_pref}: FATAL ERROR: $file is zero length"});

    recd_state($fl_name, FAILED, $tab_argv, $sched_dir, $sched_sts_fl);

    warn "FATAL ERROR: $file is zero length.\n";

    return 0;

  }

}

#******************************************************************************
sub resolveConfig
#******************************************************************************
# English Name: Resolve Configuration
# -------------
#
# Purpose: Extracts configuration parameters from the "prep_config" file
# -------- for the specified "key" and resolves embedded time tokens using
#          the date/time.
#
# Language: PERL
# ---------
#
# Usage: resolveConfig(\%config,$key)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%config                hash      OUT  returned configuration parameters
#                                        with resolved time tokens:
#
#   dataType            string      OUT  data type (key) from argument list.
#
#   remoteFile          string      OUT  remote file information in the
#                                        form: login@machine:filename
#
#   localPath           string      OUT  local pathname to store the transferred
#                                        remote file(s).
#
#   archivePath         string      OUT  local pathname to archive the
#                                        transferred remote files.
#
#   fileType            string      OUT  filetype of remote files (eg "hdf").
#                                        Default: "bufr"
#
# $key                  string       IN  data type identifier used to construct
#                                        the configuration parameter names to
#                                        be queried.
#
# $resolveConfig       boolean      OUT  function return value:
#
#                                        0: unable to retrieve one or more
#                                           configuration parameters
#
#                                        1: success
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/29/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my ($config, $key) = @_;

# Local Variables
# ---------------

  my ($rfile, $lfile, $afile);

# Make sure that configuration parameters
# are defined.
# =======================================

  if (($FILE_REMOTE  = extract_config( "FILE_${key}_REMOTE",
                              $PREP_CONFIG_FILE, "NONE" ) ) eq "NONE") {
    warn "(mget_files) ERROR - can not set FILE_${key}_REMOTE " .
                                               "configuration value\n";
    return 0;
  }

  if (($FILE_LOCAL   = extract_config( "FILE_${key}_LOCAL",
                               $PREP_CONFIG_FILE, "NONE" )) eq "NONE") {
    warn "(mget_files) ERROR - can not set FILE_${key}_LOCAL " .
                                               "configuration value\n";
    return 0;
  }

  if (($FILE_FILETYPE = extract_config( "FILE_${key}_FILETYPE",
                               $PREP_CONFIG_FILE, "NONE" )) eq "NONE") {
    print "WARNING - can not set FILE_${key}_FILETYPE configuration value. " .
                                                       "Using type \"bufr\"\n";

    $FILE_FILETYPE = "bufr";

  }

  $FILE_ARCHIVE  = extract_config( "FILE_${key}_ARCHIVE",
                                  $PREP_CONFIG_FILE, "NONE" );

  $FILE_NAME = extract_config( "FILE_${key}_NAME",
                                  $PREP_CONFIG_FILE, "NONE" );

  $FILE_PASSWD = extract_config( "FILE_${key}_PASSWD",
                                  $PREP_CONFIG_FILE, "NONE" );

# Resolve the time tokens in the
# configuration parameters.
# ==============================

  if ( $syn_missing ) {
    $rfile = token_resolve(${FILE_REMOTE},  $process_date);
    $lfile = token_resolve(${FILE_LOCAL},   $process_date);
    $afile = token_resolve(${FILE_ARCHIVE}, $process_date);
  }
  else {
    $rfile = token_resolve(${FILE_REMOTE},  $process_date, ${SYN_TIME});
    $lfile = token_resolve(${FILE_LOCAL},   $process_date, ${SYN_TIME});
    $afile = token_resolve(${FILE_ARCHIVE}, $process_date, ${SYN_TIME});
  }

  $config->{dataType}    = $key;
  $config->{remoteFile}  = $rfile;
  $config->{localPath}   = $lfile;
  $config->{archivePath} = $afile;
  $config->{name}        = $FILE_NAME;
  $config->{passwd}      = $FILE_PASSWD;
  $config->{fileType}    = $FILE_FILETYPE;

  return 1;
}

#******************************************************************************
sub getLocalHost
#******************************************************************************
# English Name: Get Local Host Information
# -------------
#
# Purpose: Queries system information on user and hostname and consolidates
# -------- local configuration parameters into the returned hash table
#
# Language: PERL
# ---------
#
# Usage: getLocalHost(\%config,\%localHost)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%config                hash       IN  hash table containing configuration
#                                        parameters.
#
#   localPath           string       IN  pathname of the directory to be used
#                                        for storing transferred files.
#
# \%localHost             hash      OUT  returned hash table containing the
#                                        local host parameters.
#
#   user                string      OUT  user name on local host.
#
#   shortName                       OUT  hostname with domain information
#
#   longName                        OUT  hostname with domain information
#
#   workPath                        OUT  local destination directory for
#                                        transferred files.
#
#   archivePath                     OUT  archive directory for transferred
#                                        files.
#
# $getLocalHost        boolean      OUT  function return value:
#
#                                        0: error retrieving local host info
#                                        1: success
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/29/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my ($config, $localHost) = @_;

# Local Variables
# ---------------

  my $host;

# Retrieve host and user information.
# ===================================

  $localHost->{user}      = eval { (getpwuid($>))[0] } || $ENV{HOME};

  $host = hostname();
  $localHost->{shortName} = $host;
  $localHost->{longName}  = ( gethostbyname( $host ) )[0];

  $localHost->{workPath}    = $config->{localPath};
  $localHost->{archivePath} = $config->{archivePath};

  return 1;
}

#******************************************************************************
sub getRemoteHost
#******************************************************************************
# English Name: Get Remote Host Information
# -------------
#
# Purpose: Extracts remote host access information from the configuration  
# -------- parameters.
#
# Language: PERL
# ---------
#
# Usage: getRemoteHost(\%config,\%remoteHost)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%config                hash       IN  hash table containing configuration
#                                        parameters.
#
#   remoteFile          string       IN  remote file information in the
#                                        form: login@machine:filename
#
# \%remoteHost            hash      OUT  returned hash table containing the
#                                        remote host parameters.
#
#   login               string      OUT  user name on local host.
#
#   machine                         OUT  hostname with domain information
#
#   file                            OUT  hostname with domain information
#
# $getRemoteHost       boolean      OUT  function return value:
#
#                                        0: error retrieving remote host info
#                                        1: success
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#******************************************************************************
{

# Argument List
# -------------

  my ($config, $remoteHost) = @_;

# Parse out the remote server information
# that is encoded in the remote file
# configuration parameter.
# =======================================

  my @fields;

# HTTP Protocol

  @fields = split /:\/\//, $config->{remoteFile};

  $fields[0] =~ /^http$/i and do {

    my $path = dirname $fields[1];
    my $name = basename $fields[1];

    $remoteHost->{url}    = "http://" . $path;
    $remoteHost->{file}   = $name;
    $remoteHost->{name}   = $config->{name};
    $remoteHost->{passwd} = $config->{passwd};

    return 1;

  };

# FTP Protocol

  my ($login, $machine, $file);

  @fields = split /@/, $config->{remoteFile};
  $login  = $fields[0];

  @fields  = split /:/, $fields[1];
  $machine = $fields[0];
  $file    = $fields[1];

  $remoteHost->{login}   = $login;
  $remoteHost->{machine} = $machine;
  $remoteHost->{file}    = $file;

  return 1;
}

#******************************************************************************
sub transferFiles
#******************************************************************************
# English Name: Transfer Files
# -------------
#
# Purpose: Transfers files from a remote location using the FTP protocol. 
# -------- 
#
# Language: PERL
# ---------
#
# Usage: transferFiles(\%remoteHost, \%localHost)
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%remoteHost            hash       IN  hash table containing the remote
#                                        host parameters.
#
#   login               string       IN  user name on local host.
#
#   machine                          IN  hostname with domain information
#
#   file                             IN  hostname with domain information
#
# \%localHost             hash       IN  hash table containing the local
#                                        local host parameters.
#
#   user                string       IN  user name on local host.
#
#   longName                         IN  hostname with domain information
#
#   workPath                         IN  local destination directory for
#                                        transferred files.
#
# @transferFiles         array      OUT  function return value: list of 
#                                        transferred files or empty list if
#                                        unable to complete the operation.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/30/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my ($remoteHost, $localHost) = @_;

# Local Variables
# ---------------

  my $ftp;
  my $i = 0;
  my $attempt;
  my ($login, $machine, $passwd);
  my ($user, $hostname, $workPath);
  my ($remoteFile, $localFile);
  my (@remoteList, @localList);

# Extract remote and local host information
# =========================================

  $machine     = $remoteHost->{machine};
  $login       = $remoteHost->{login};
  $remoteFile  = $remoteHost->{file};

  $user     = $localHost->{user};
  $hostname = $localHost->{longName};
  $workPath = $localHost->{workPath};

  mkpath("$workPath");

# User-based password is assumed for now.
# For anonymous FTP, this is usually an
# email address. Here it is set to "user@hostname".
# -------------------------------------------------

  $passwd = join("@",$user,$hostname);

# Establish a connection to the remote host
# and retrieve a listing of files matching
# the filename template. Make several attempts
# before returning and error.
# ============================================

  for ($attempt=1; $attempt<6; $attempt++) {

    print "Attempting to connect to the FTP server: Attempt #$attempt\n";

    $ftp = Net::FTP->new("$machine") or next;
    $ftp->login("$login","$passwd") or next;

    print "FTP connection established: $machine\n";

    last;

  }

  if ($attempt >= 6) {

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
       {'err_desc' => "${err_pref}: Running on $hostname - " .
                        "Unable to connect to $machine."});

    return ();

  }

  $ftp->binary();
  @remoteList = $ftp->ls("$remoteFile");

  if (! @remoteList) {

    print "Files not found.\n";

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
       {'err_desc' => "${err_pref}: Running on $hostname - " .
                        "Files not found."});

    return ();

  }

# Transfer the files. Retain the same filename.
# =============================================

  foreach (@remoteList) {

    print "$_\n";
    $localFile = "$workPath" . "/" . basename($_);
    $ftp->get($_,$localFile);

    isTransferred($localFile) or return ();

    $localList[$i++] = $localFile;

  }

  $ftp->quit;

  return @localList;
}
#******************************************************************************
sub wgetFiles
#******************************************************************************
# English Name: Transfer Files Using WGET
# -------------
#
# Purpose: Transfers files from a remote location using the HTTP protocol. 
# -------- 
#
# Language: PERL
# ---------
#
# Usage: wgetFiles(\%remoteHost, \%localHost)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%remoteHost            hash       IN  hash table containing the remote
#                                        host parameters.
#
#   url                 string       IN  remote URL source for data.
#
#   file                             IN  name or pattern of desired files.
#
# \%localHost             hash       IN  hash table containing the local
#                                        local host parameters.
#
#   workPath                         IN  local destination directory for
#                                        transferred files.
#
# @wgetFiles             array      OUT  function return value: list of 
#                                        transferred files or empty list if
#                                        unable to complete the operation.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/31/2014      J.Ardizzone  Adapted from transferFiles()
#******************************************************************************
{

# Argument List
# -------------

  my ($remoteHost, $localHost) = @_;

# Extract remote and local host information
# =========================================

  my $url        = $remoteHost->{url};
  my $remoteFile = $remoteHost->{file};
  my $user       = $remoteHost->{name};
  my $passwd     = $remoteHost->{passwd};
  my $workPath   = $localHost->{workPath};

  print "wget -q -O - $url\n";
  open my $fh, "wget -q -O - $url 2> /dev/null |";

  while (<$fh>) {
    chomp;
    />($remoteFile)<\/a>/ and do { push @remoteList, $1};
  }

  close $fh;

  if (! @remoteList) {

    print "Files not found.\n";

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
       {'err_desc' => "${err_pref}: Running on $hostname - " .
                        "Files not found."});

    return ();

  }

# Transfer the files.
# ===================

  mkpath $workPath if ! -d $workPath;
  chdir $workPath;

  print "Transferring the files using wget\n";

  my $wget;

  if ($passwd ne "NONE") {
    $wget = "wget -q --user $user --password $passwd -r -l1" .
                    " --no-parent -A'$remoteFile' -nH -nd -nc $url";
  }
  else {
    $wget = "wget -q -r -l1 --no-parent -A'$remoteFile' -nH -nd -nc $url";
  }

  system "$wget" and do {

    print "Error returned from wget.\n";

    err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1",
             {'err_desc' => "${err_pref}: Running on $hostname - " .
                                         "Error returned from wget"});

    return ();

  };

# Verify the transfer.
# ====================

  my ($localFile, @localList);

  foreach (@remoteList) {

    print "$_\n";
    $localFile = "$workPath" . "/" . basename($_);

    isTransferred($localFile) or return ();

    push @localList, $localFile;

  }

  return @localList;
}
#******************************************************************************
sub archiveFiles
#******************************************************************************
# English Name: Archive Files
# -------------
#
# Purpose:
# --------
#
# Language: PERL
# ---------
#
# Usage: archiveFiles(\%config, @fileList)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%config                hash    INOUT  configuration parameters.
#
#   dataType            string       IN  data type (key) from argument list.
#
#   archivePath         string       IN  local pathname to archive the
#                                        transferred remote files.
#
#   fileType            string       IN  filetype of files (eg "hdf").
#                                        Default: "bufr"
#
#   failedArchive      boolean      OUT  flag indicating archive failure.
#
# @fileList             string       IN  list of transferred files on local
#                                        machine.
#
# $archiveFiles        boolean      OUT  function return value:
#
#                                        0: error occurred
#                                        1: success
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           01/31/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my ($config, @fileList) = @_;

# Local Variables
# ---------------

  my ($rc, $key, $archivePath, $fileType);

# Extract configuration parameters.
# =================================

  $key          = $config->{dataType};
  $fileType     = $config->{fileType};
  $archivePath  = $config->{archivePath};

# Exit if the archive path is undefined
# or if there are no files to archive.
# ======================================

  if ("$archivePath" eq "NONE") { return 1};
  if (! @fileList) { return 1}; 

# Generate the archive. Option "D" turns off
# automatic defaults and designates the
# archive path as the path to be used without
# ops augmentation.
# ===========================================

  if ( $opt_D ) {

    $rc = gen_archive ( 'ops', $prep_ID, ${opt_n}, $fileType, 
                        "$process_date", "$archivePath", @fileList,
            { 'subtype' => "$key",'exp_path' => "1", 'verbose' => "1" } );

  }
  else {

    $rc = gen_archive ( 'ops', $prep_ID, ${opt_n}, $fileType, 
                         "$process_date", "$archivePath", @fileList,
                            { 'subtype' => "$key", 'verbose' => "1" } );

  }

  if ($rc >= 1) { return 1 };

# Log archive failures.
# =====================

  err_log (4, "mget_files.pl", "$date_time_log","$prep_ID","-1", 
                       {'err_desc' => "${err_pref}: Running on $host " .
                        "- could not archive files for file type \"$key\". " .
                                              "Will continue processing\n"});

  $config->{failedArchive} = 1;

  return 1;
}

#******************************************************************************
sub archiveListing
#******************************************************************************
# English Name: Archive Listing
# -------------
#
# Purpose: Archives the job listings file.
# --------
#
# Language: PERL
# ---------
#
# Usage: archiveListing(\%config,$listing)
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $listing              string       IN  name of listing file.
#
# \%config                hash      OUT  configuration parameters.
#
#   failedArchive      boolean      OUT  flag indicating archive failure.
#
# $archiveListing      boolean      OUT  function return value:
#
#                                        0: error during archive
#                                        1: success
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/01/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my ($config, $listing) = @_;

# Local Variables
# ---------------

  my $rc;

# Option -D signifies that ops pathname
# conventions are not being followed hence there
# is no way to predict where the listing file
# should go. Do nothing and return.
# ==============================================

  if ($opt_D) {return 1};

# If option -O was not specified, then there is
# no saved listing information. Do nothing and
# return.
# =============================================

  if (!$opt_O) {return 1};

# Archive the listing file in the directory
# defined by "$FILE_ARCHIVE". This directory is
# assumed to be constant and independent of
# of data type (key) and time.
# =============================================

  $rc = gen_archive ( 'ops', $prep_ID, ${opt_n}, 'listings', 
                      "$process_date","$FILE_ARCHIVE", $listing, 
                    { 'remote_name' => "${opt_n}.$process_date.listing",
                      'delete' => "0" } );

  if ($rc != 1) {

    err_log (4, "mget_files.pl", "${date_time_log}","$prep_ID","-1",
           {'err_desc' => "${err_pref}: WARNING: Running on $host - " .
           "could not archive $listing to $FILE_ARCHIVE"});

    print "\nCould not archive $listing to $FILE_ARCHIVE\n";

    config->{failedArchive} = 1;

    return 0;
 
  }

  return 1;
}

#******************************************************************************
sub cleanup
#******************************************************************************
# English Name: Cleap-up
# -------------
#
# Purpose: Performs final tasks and updates the error log and task status
# -------- files with concluding remarks.
#
# Language: PERL
# ---------
#
# Usage: cleanup(\%config)
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# \%config                hash       IN  configuration parameters.
#
#   failedArchive      boolean       IN  flag indicating archive failure.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/01/2013      J.Ardizzone  (adapted from get_files.pl)
#******************************************************************************
{

# Argument List
# -------------

  my $config = shift;

# Rename output listing
# =====================

  if ( "$listing_file" ne "STDOUT")
    {system ("mv $listing_file $opt_O/${opt_n}.${date_time_log}.listing");}

# Make concluding remarks in the
# error log file.
# ==============================

  if ( $config->{failedArchive} ) {

    err_log (4, "mget_files.pl", "${date_time_log}","$prep_ID","-1",
           {'err_desc' => "${err_pref}: Running on $host - exiting with " . 
                                                            "archive errors"});
  }
  else {

    err_log (0, "mget_files.pl", "${date_time_log}","$prep_ID","-1",
           {'err_desc' => "${err_pref}: Running on $host - exiting normally"});
  }

# Terminate the task status file with a 
# final remark indicating completion of the job.
# ==============================================

  if ( defined( $sched_id ) && $tab_status != 0 )
    {recd_state( $fl_name, "COMPLETE", $tab_argv, $sched_dir, $sched_sts_fl );}

  print "Finished.\n";
}
