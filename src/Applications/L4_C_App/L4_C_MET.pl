#!/usr/bin/perl

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
  use TMGR;
  use DMGR;
  use SYSTEM;
  use LOG::Handler;
  use ERROR::Handler;

# Local Variables
# ---------------

  STRING:  my $this = "main";

  TMGR:    my $tmgr;
  SYSTEM:  my $sys;
  CONFIG:  my $config;

  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

# Retrieve run-time input
# parameters
# =======================

  $config = getRunTimeConfig();
  exit 1 if $eh->isError();

  $tmgr = TMGR->new($config->{L4_C_MET});
  $tmgr->isClockRunning() or exit 0;

# Retrieve the execution state
# of the system.
# ============================

  $sys = querySystem($config);
  exit 2 if $eh->isError();

  $sys->isEnabled() or exit 0;
  exit 0 if $sys->isBusy();

# Import data for L4_C Met
# Preprocessor
# ========================

  import($config);
  exit 3 if $eh->isError();

# Execute the L4_C_MET
# application.
# ====================

  runMet($config);
  exit 4 if $eh->isError();

  exit 0;

  $sys->isaSuccess() or exit 0;

# Update the system clock and
# reset the system for processing
# a new date/time.
# ================================

  $eh->comment($this,"L4_C_MET is complete.");

  $tmgr->updateClock();
  $sys->closeProcess();

exit 0;

sub import
{ 
  CONFIG: my $config = shift; 

  STRING: my $path;
  STRING: my $import = $config->{L4_C_IMPORT};
  SYSTEM: my $sys = SYSTEM->new($config->{L4_C_MET});

  $sys->isReady() or return;

  $path = File::Spec->catdir($import,"l4c_met");
  manage_data($path, $config );

  $path = File::Spec->catdir($import,"ancillary");
  manage_data($path, $config );

  $path = File::Spec->catdir($import,"restart","soc");
  manage_data($path, $config );
}

sub manage_data
{

# Argument List
# -------------

  STRING: my $path   = shift;
  CONFIG: my $config = shift;

# Local Variables
# ---------------

  STRING: my $this   = "manage_data";

  STRING: my $type;
  DMGR:   my $transaction;

  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

# Execute the transactions
# ========================

  $transaction = DMGR->new($path,%$config);
  foreach $type (keys %$transaction) { $transaction->execute($type) }

# Return if all transactions have
# completed. An expired transaction
# is considered complete.
# ======================================

  return if isComplete($transaction);

# Issue warning if one or more
# transactions are still pending.
# ===============================

  return $eh->warning($this,1,THROW=>1);
}

sub runMet
{

# Argument List
# -------------
  
  CONFIG: my $config = shift;

# Local Variables
# ---------------

  STRING: my $this   = "runMet";

  DMGR:   my $dtr;
  STRING: my $type;
  HASH:   my %options = ();
  DMGR:   my $transaction;

  SYSTEM:        my $sys = SYSTEM->new($config->{L4_C_MET},%$config);
  ERROR_HANDLER: my $eh  = ERROR::Handler->new(\&error_handler);

# Nothing to do if the run
# succeeded.
# ========================

  return if $sys->isaSuccess();

# Check the status of all inputs.
# ===============================

  $transaction = DMGR->new($config->{L4_C_IMPORT},%$config);

  foreach $type (qw/MET_FP MET_L4_SM MET_L3_SM_A SOC ANC/) {

    $dtr     = $transaction->{$type};
    print "$dtr->{MET_FP_ROOT} $dtr->{DTR_DIR_ID}\n";
    %options = (%options, %$dtr);
    ($transaction->stat($type))[0] or $eh->error($this,1,TYPE=>$type);

  }

  return $eh->throw() if $eh->isError();

# Configure the model and
# submit the batch job.
# =======================

  $config->jobConfig($config->{L4_C_TMPL},$config->{L4_C_RUN},%options);

# Allocate a new process for
# running the model and submit the
# batch job.
# ================================

  $sys->openProcess() or return $eh->error($this,1);

  $eh->comment($this,"Submitting L4c_MET job");
# $sys->submitBatch($run_path);

}

sub isComplete
{

# Argument List
# -------------

  DMGR: my $transaction = shift;

# Local Variables
# ---------------

  STRING:  my $this = "isComplete";

  STRING:  my $key;
  DTR:     my $dtr;
  BOOLEAN: my $status = 1;
  BOOLEAN: my ($success, $expired, $enabled);

  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

  foreach $key (keys %$transaction) {

    $dtr = $transaction->{$key};
    ($success, $expired, $enabled) = $transaction->stat($key,%$dtr);

    $enabled or next;
    print "$dtr->{PDR_SHORT_NAME}: $success\n";

    $success and next;

#   Issue warnings and set status

    if ( $expired ) { $eh->warning($this,1,%$dtr); next }

    $status = 0;

  }

  return $status;
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

  STRING:  my $path;
  ANY:     my %option;
  CONFIG:  my $config;
  TMGR:    my $tmgr;

  INTEGER: my ($PRODUCT_COUNTER, $prod_counter);

  $option{NAME}  = "SMAP_L4_OPS";
  $option{CLASS} = "L4_C";

# Retrieve command-line arguments
# ===============================

  GetOptions ('n=i',\$prod_counter);
  $option{PRODUCT_COUNTER} = $prod_counter // 1;
  $option{PRODUCT_COUNTER} = substr($option{PRODUCT_COUNTER}+1000, 1);

# @ARGV == 3 or usage();

  $path             = $ARGV[0];

# Derive key parameters
# =====================

  $option{L4_STREAM}       = basename $path;
  $option{L4_INSTALL_ROOT} = dirname $path;
  $option{JOBNAME}         = $option{L4_STREAM} . "_C";

  $option{L4_ROOT}     = $path;
  $option{L4_C_ROOT}   = File::Spec->catdir($option{L4_ROOT},   "L4_C");
  $option{L4_C_PATH}   = File::Spec->catdir($option{L4_C_ROOT}, "bin");
  $option{L4_C_IMPORT} = File::Spec->catdir($option{L4_C_ROOT}, "import");
  $option{L4_C_MET}    = File::Spec->catdir($option{L4_C_ROOT}, "run_MET");
  $option{L4_C_TMPL}   = File::Spec->catdir($option{L4_C_MET},  "templates");
  $option{L4_C_RUN}    = File::Spec->catdir($option{L4_C_MET},  "run");

  my $log_file         = File::Spec->catdir($option{L4_C_ROOT}, "log");
  $ENV{L4_LOG_FILE}    = $ENV{L4_LOG_FILE} // $log_file;
  $option{L4_LOG_FILE} = $ENV{L4_LOG_FILE};

  $ENV{PATH}     = join ":", $option{L4_C_PATH}, $ENV{PATH};

# Retrieve the date/time
# from the system clock.
# ======================

  $tmgr = TMGR->new($option{L4_C_MET});
  $option{DATE} = $tmgr->{DATE};
  $option{TIME} = $tmgr->{TIME};
  $ENV{DATE}    = $tmgr->{DATE};
  $ENV{TIME}    = $tmgr->{TIME};

# Return the run-time configuration
# =================================

  $config = CONFIG->new($option{L4_C_MET},%option);

  return $config;

}

#******************************************************************************
sub querySystem
#******************************************************************************
# English Name: Query System
# -------------
#
# Purpose: Checks the operational status of the model for the date/time being
# -------- processed.
#
# Language: Perl
# ---------
#
# Usage: $sys = querySystem(%config)
# ------ $eh->isError()
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# %config                 hash       IN  Run-time configuration parameters
#                                        [see getRunTimeConfig()].
#
# $boolean             boolean      OUT  function return value:
#
#                                        0: model is not running for date/time.
#                                        1: model is running.
#
# $eh                    ERROR      OUT  exception handler (see ERROR.pm).
#                                        
#                                        Thrown Exception:
#
#                                        1: model run has exceeded expected
#                                           wall-time for completion (warning).
#
#                                        2: processing options for the current
#                                           date/time have expired (fatal).
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           09/16/2013      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# =============

  CONFIG: my $config = shift;

# Local Variables
# ===============

  STRING:  my $this = "querySystem";

  STRING:  my $dir;
  DTR:     my $dtr;
  DMGR:    my $transaction;
  STRING:  my $path = $config->{L4_C_MET};
  TMGR:    my $tmgr = TMGR->new($path);
  STRING:  my $jobname = $config->{JOBNAME};
  SYSTEM:  my $sys  = SYSTEM->new($path,JOBNAME=>$jobname);
  INTEGER: my $date = $tmgr->{DATE};
  INTEGER: my $time = $tmgr->{TIME};
  INTEGER: my ($success, $expired, $inQueue);

  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

# Simply return if the process
# has been disabled.
# ============================

  $sys->isEnabled() or return $sys;

# Check the processing status for the
# current date/time.
# ===================================

  $dir = File::Spec->catdir($path,"output");
  $transaction = DMGR->new($dir,%$config);

  $dtr = $transaction->{L4_C_MET};
  $dtr or return $eh->error($this,1,TYPE=>"L4_C_MET");

  ($success, $expired) = $transaction->stat("L4_C_MET",DATE=>$date);
# ($inQueue) = $sys->queryBatch();

# Return if the model 
# completed successfully.
# =======================

  if ($success) { $sys->setStatus(SUCCESS=>1); return $sys }

# Advise and return if the job is still
# running.
# =====================================

# if ($expired && $inQueue) { $eh->warning($this,2); return $sys }
# $inQueue and return $sys;

  if ($expired) { $eh->warning($this,2); return $sys }

# Set the failure bit since no success
# and the job is not in the queue.
# ====================================

  $sys->setStatus(FAILED=>1);

  return $sys;

}

sub error_handler
{
  my $error_handle = shift;
  my $error_code = shift;
  my %options = scalar(@_) ? @_ : ();

  my $lh = LOG::Handler->new();

  $lh->{SYSTEM}      = "SMAP_L4_OPS";
  $lh->{APPLICATION} = "L4_C";
  $lh->{HANDLE}      = $error_handle;
  $lh->{DATE}        = $ENV{DATE};
  $lh->{TIME}        = $ENV{TIME};

  print "LOG_FILE    = $ENV{LOG_FILE}\n";

  $error_code or return $lh->comment(0,$options{COMMENT});

  given ($error_handle) {

    when ("querySystem") {

      given ($error_code) {

        when (1) { $lh->error(1,"No data transaction for type, " .
                                "\"$options{TYPE}\".") }

        when (2) { $lh->warning(2,"L4_C_MET has exceeded the expected " .
                                  "walltime. Job still in queue.") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("import") {

      given ($error_code) {

        when (1) { $lh->warning(1,"Data import exiting with errors.") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("export") {

      given ($error_code) {

        when (1) { $lh->warning(1,"Data export exiting with errors.") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("runMet") {

      given ($error_code) {

        when (1) { $lh->error(2,"Required inputs are missing: " .
                                "\"$options{TYPE}\".") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("querySystem") {

      given ($error_code) {

        when (1) { $lh->error(1,"No data transaction for type, " .
                                "\"$options{TYPE}\".") }

        when (2) { $lh->warning(2,"The LDAS has exceeded the expected " .
                                "walltime.") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("isComplete") {

      given ($error_code) {

        when (1) { $lh->warning(1,"Unable to complete data transaction for " .
                                "\"$options{PDR_SHORT_NAME}\": " .
                                "$options{PDR_LONG_NAME}") }

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    when ("manage_data") {

      given ($error_code) {

        when (1) { $lh->warning(1,"Waiting for one or more data transaction " .
                                "to complete.") }

        default { print STDERR "$error_handle: Unknown error ", 
                               "code: $error_code\n" }

      }

    }

    when ("main") {

      given ($error_code) {

        default { print STDERR "$error_handle: Unknown error ",
                               "code: $error_code\n" }

      }

    }

    default { print STDERR "L4_C_MET: no error handle for ",
                           "\"$error_handle\"\n" }

  }

}
