#!/usr/bin/perl

#******************************************************************************
# English Name: Correct Precipitation
# -------------
#
# Purpose: This is the main driver for performing all functions necessary to 
# -------- gather input data and execute the precipitation correction
#          algorithm.
#
# Language: Perl
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           mm/dd/ccyy      J.Ardizzone  created.
#******************************************************************************

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use Time::Seconds;
  use feature qw(switch say);

  use CONFIG;
  use CLOCK;
  use FIND;
  use DTR;
  use DMGR;
  use SYSTEM;
  use LOG::Handler;
  use ERROR::Handler;

  FUNCTION_NAME: my $this = "correctPrecip";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

# Retrieve run-time input
# parameters
# =======================

  CONFIG: my $config = getRunTimeConfig();
  exit 1 if $eh->isError();

  TMGR: my $tmgr = TMGR->new($config->{PCORR_ROOT});
  $tmgr->isClockRunning() or exit 2;

  $tmgr->tickClock(3600);

# Exit if the correction process
# is turned off.
# ==============================

  SYSTEM: my $sys = SYSTEM->new($config->{PCORR_MODEL});
  $sys->isEnabled() or exit 0;

# Gather input/output information
# ===============================

  DMGR: my $input  = DMGR->new($config->{PCORR_IMPORT},%$config);
  DMGR: my $output = DMGR->new($config->{PCORR_OUTPUT},%$config);

  ($output->mstat())[0] and exit 0;

# Execute the correction
# algorithm.
# ======================

  $eh->comment($this,"Begin Precipitation Correction");

  my $rc;

  $rc = import($input);
  exit 2 if $eh->isError();

  $rc or exit 1;

  $rc = runModel($input, $output, $config);
  exit 3 if $eh->isError();

  $eh->comment($this,"Precipitation Correction is complete.");

# Update the clock
# ================

  $tmgr->updateClock();

exit 0;

#******************************************************************************
sub import { DMGR: my $input = shift;
#******************************************************************************
# English Name:
# -------------
#
# Purpose:
# --------
#
# Language: Perl
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           mm/dd/ccyy      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this = "import";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

  $input->mexecute() or return 0;

  foreach my $type (keys %$input)
  { ($input->stat($type))[0] or $eh->error($this,1,TYPE=>$type) }

  ($input->mstat())[0] or return $eh->error($this,2);

  return 1;
}

#******************************************************************************
sub runModel { DMGR: my $input = shift;
               DMGR: my $output = shift;
               CONFIG: my $config = shift;
#******************************************************************************
# English Name: Run Model (Model = Precipitation Correction Algorithm)
# -------------
#
# Purpose: Executes the precipitation correction algorithm using the supplied
# -------- configuration parameters. 
#
# Language: Perl
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           04/20/2014      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this = "runModel";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

  PATH: my $run_path = $config->{PCORR_RUN};
  PATH: my $rip_path = $config->{PCORR_RIP};

# Retrieve input files and filename
# templates.
# =================================

  DTR: my $dtr;

  $dtr = $input->{LFO_INST};
  $ENV{LFO_INST_FILENAME_TEMPLATE} = ($dtr->files())[0];

  $dtr = $input->{AFRICA_MASK};
  $config->{AFRICA_MASK_FILE} = ($dtr->files())[0];

  $dtr = $input->{LFO_TAVG};
  $config->{LFO_TAVG_FILENAME_TEMPLATE} = ($dtr->files())[0];

  $dtr = $input->{CPCU_PRECIP};
  $config->{CPCU_PRECIP_FILENAME_TEMPLATE} = ($dtr->files())[0];

  $dtr = $input->{CPCU_EOD};
  $config->{CPCU_EOD_FILE} = ($dtr->files())[0];

  $dtr = $input->{CPCU_SCALE};
  $config->{CPCU_SCALE_FILE} = ($dtr->files())[0] if $dtr;

  $dtr = $input->{LFO_SCALE};
  $config->{LFO_SCALE_FILE} = ($dtr->files())[0] if $dtr;

  $dtr = $output->{LFO_CORR};
  $config->{LFO_CORR_FILENAME_TEMPLATE} = ($dtr->files())[0];

# Pre-create output directories
# spanning 2-days.
# =============================

  my $dir;
  my $time = CLOCK->new(%$config);
  my $out_dir = dirname $config->{LFO_CORR_FILENAME_TEMPLATE};

  $dir = $time->strftime($out_dir);
  makepath($dir) or return $eh->fatal($this,1,PATH=>$dir);

  $time += 86400;
  $dir = $time->strftime($out_dir);
  makepath($dir) or return $eh->fatal($this,1,PATH=>$dir);

# Resolve runtime input parameter
# files.
# ===============================

  makepath($run_path) or return $eh->fatal($this,1,PATH=>$run_path);
  $config->jobConfig($rip_path,$run_path,TIMELESS=>1); 

# Execute the correction algorithm
# ================================

  my $date = $config->{DATE};
  my $resource = File::Spec->catfile($run_path, "corrector.rc");

  system "correct_precip.x $resource $date $date" and 
                              return $eh->error($this,2);

  $output->mexecute();
  ($output->stat("LFO_CORR"))[0] or return $eh->error($this,3);

  return 1;
}

#******************************************************************************
sub getRunTimeConfig
#******************************************************************************
# English Name: Get Run-Time Configuration
# -------------
#
# Purpose: 
# --------
#
# Language: Perl
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           mm/dd/ccyy      J.Ardizzone  created.
#******************************************************************************
{

# Local Variables
# ===============

  ANY:     my %option;
  CONFIG:  my $config;

  $option{NAME}  = "SMAP_L4_OPS";
  $option{CLASS} = "PCORR";

  my $path = $ARGV[0];
  my $date = $ARGV[1];
  my $time = $ARGV[2];

# Derive key parameters
# =====================

  $option{PCORR_STREAM}  = basename $path;

  $option{PCORR_ROOT}    = $path;
  $option{PCORR_PATH}    = File::Spec->catdir($option{PCORR_ROOT}, "bin");
  $option{PCORR_IMPORT}  = File::Spec->catdir($option{PCORR_ROOT}, "import");
  $option{PCORR_MODEL}   = File::Spec->catdir($option{PCORR_ROOT}, "correctPrecip");
  $option{PCORR_RIP}      = File::Spec->catdir($option{PCORR_MODEL}, "rip");
  $option{PCORR_RUN}      = File::Spec->catdir($option{PCORR_MODEL}, "run");
  $option{PCORR_OUTPUT}   = File::Spec->catdir($option{PCORR_MODEL}, "output");

  my $log_file         = File::Spec->catdir($option{PCORR_ROOT},  "log");
  $ENV{L4_LOG_FILE}    = $ENV{L4_LOG_FILE} // $log_file;
  $option{L4_LOG_FILE} = $ENV{L4_LOG_FILE};

  $ENV{PATH}     = join ":", $option{PCORR_PATH}, $ENV{PATH};

# Retrieve the date/time
# from the system clock.
# ======================

# $option{DATE} = $date;
# $option{TIME} = $time;
# $ENV{DATE}    = $date;
# $ENV{TIME}    = $time;

# Retrieve the date/time
# from the system clock.
# ======================

  my $tmgr = TMGR->new($option{PCORR_ROOT});
  $option{DATE} = $tmgr->{DATE};
  $option{TIME} = $tmgr->{TIME};
  $ENV{DATE}    = $tmgr->{DATE};
  $ENV{TIME}    = $tmgr->{TIME};

# Return the run-time configuration
# =================================

  $config = CONFIG->new($option{PCORR_ROOT},%option);

  return $config;

}

sub makepath { my $dir = shift; return 1 if -d $dir; mkpath $dir or return 0 }

sub error_handler
{
  my $error_handle = shift;
  my $error_code = shift;
  my %options = scalar(@_) ? @_ : ();

  my $lh = LOG::Handler->new();

  $lh->{SYSTEM}      = "SMAP_L4_OPS";
  $lh->{APPLICATION} = "correctPrecip";
  $lh->{HANDLE}      = $error_handle;
  $lh->{DATE}        = $ENV{DATE};
  $lh->{TIME}        = $ENV{TIME};


# Pre-create output directories
# spanning 2-days.
# =============================

  $error_code or return $lh->comment(0,$options{COMMENT});

  given ($error_handle) {

    when ("runModel") {

      given ($error_code) {

        when (1) { $lh->fatal("FSYS-001","Unable to create directory: " .
                                   "\"$options{PATH}\".") }

        when (2) { $lh->error("PCORR-001","Error returned from " .
                               "correction algorithm") }

        when (3) { $lh->error("PCORR-002","Output from correction " .
                               "algorithm is incomplete") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("import") {

      given ($error_code) {

        when (1) { $lh->error("PCORR-003","Missing input data: " .
                               "\"$options{TYPE}\"") }

        when (2) { $lh->error("PCORR-004","Required input files are missing") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    default { print STDERR "correctPrecip: no error handle for ",
                           "\"$error_handle\"\n" }

  }

}
