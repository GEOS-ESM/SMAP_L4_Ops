#!/usr/bin/perl

#******************************************************************************
# English Name: Level-4 Carbon Science Processing System (SPS)
# -------------
#
# The text following this prolog contains documentation for this application
# using Perl POD markup language. Formatted documentation can be obtained
# using the following commands:
#
#        perldoc L4_C.pl
#        L4_C.pl -h|-help
#        L4_C.pl -man
#
#******************************************************************************

=head1 NAME

L4_C.pl - Executes a SMAP Level-4 Carbon (L4_C) data stream.

=head1 SYNOPSIS

L4_C.pl [-h|help] [-man] [-clim] [-export] [-f|force] [-noHDF] [-noHold] [-pc counter] [-rf|run_freq interval] [-sn|stream_name name] [-st|stream_type type]

=head1 OPTIONS

=over 17

=item -h|help

Print a brief help message and exit.

=item -man

Print this command's manual page and exit.

=item -export

Automatically export generated granules to subscribing clients of the Level-4 carbon data. Exporting can also be performed manually using the SMAP Level-4 dashboard command. Please note that PDRs describing the exported products may need to be manually moved to the designated server location for each client. This allows inspection of the PDRs before making them visible to the clients. See also: -noHold.

=item -f|force

Force processing to proceed. Wait times for pending data are ignored. However, processing cannot proceed when primary input data types are missing.

=item -clim

Force FPAR climatology to be used instead of MODIS. This option must be used with -f in order to override the MODIS wait logic.

=item -noHDF

Turn HDF processing off. Level-4 granules will not be automatically generated. Use L4_C_genISO.pl to generate granules as a separate task.

=item -noHold

Do not hold PDRs when -export is specified. Use this option to send PDRs directly to the polled directory for immediate transmission to clients without inspection.

=item -pc counter

Set the product counter. I<counter> is an integer between 1 and 999. The default
value is 1. Please reference L4_C_genISO.pl for more information.

=item -rf|run_freq interval

Set the run frequency. The I<interval> is specified in seconds and is expected to match the spacing between invocations of this application. The default is 3600 seconds.

=item -sn|stream_name name

Set the stream name, which is also the name of the last directory in the
fully qualified path location of the stream.

=item -st|stream_type type

Set the stream type, which is also the last parent directory in the fully
qualified path location of the stream.

=back

=head1 DESCRIPTION

Executes a SMAP Level-4 Carbon (L4_C) data stream. This is the main driver for end-to-end processing including data acquisition, model execution, HDF-5 generation and export of final SMAP Level-4 carbon products.

=head1 NOTES

=over 4

=item 1.

This application requires a configured L4_C data stream. The following documentation is available:

 "SMAP Level-4 SPS User Guide"
 "SMAP Level-4 Carbon SPS User Guide"

=item 2.

This application must be periodically invoked to complete processing. It is typically managed as a CRON job or background daemon process. It can also run under the Dependency-Based Operational System Scheduler (D-BOSS).

=item 3.

The L4 SPS keeps a record of completed tasks. This application will simply exit if the system records indicate that processing was completed for the specified date/time. Use the L4 SPS dashboard command (L4.pl) for rewinding or reprocessing. Please reference the L4 SPS documentation for more information.

=back

=head1 SEE ALSO

L4_C_genISO.pl(1), L4.pl(1)

=head1 AUTHORS

Joseph V. Ardizzone - NASA Goddard Space Flight Center, Global Modeling and Assimilation Office (GMAO)

=head1 COPYRIGHT

This software is the property of the National Aeronautics and Space
Administration (NASA) and is subject to the regulations contained in the NASA
Procedural Requirements document NPR 2210.1C managed by the Office of the Chief
Technologist.

=cut

#******************************************************************************
# English Name: Level-4 Carbon SPS - Main Driver
# -------------
#
# Purpose: Executes a SMAP Level-4 carbon (SPL4C) data stream. This is
# -------- the main driver for end-to-end processing including data acquisition,
#          model execution, HDF generation, ISO metadata and export of final
#          SMAP Level-4 carbon (L4_C) products.
#
# Language: Perl
# ---------
#
# Notes: 1. This application requires a configured L4_C data stream. The
# ------    following documentation is available:
#
#           "SMAP Level-4 SPS User Guide"
#           "SMAP Level-4 Carbon SPS User Guide"
#
#        2. This application must be periodically invoked to complete 
#           processing. It is typically managed as a CRON job or background
#           daemon process (see L4_C.d). It can also run under the 
#           Dependency-Based Operational System Scheduler (D-BOSS).
#
#        3. The L4 SPS keeps a record of completed tasks. This application will
#           simply exit if the system records indicate that processing was
#           completed for the specified date/time. Use the L4 SPS dashboard
#           command (L4.pl) for rewinding or reprocessing. Please reference
#           the L4 SPS documentation for more information. 
#
# See Also: DMGR.pm, CONFIG.pm, DTR.pm, CLOCK.pm, FIND.pm, TMGR.pm,
# --------  SYSTEM.pm, METADATA.pm, ERROR::Handler.pm, LOG::Handler.pm,
#           L4_C_genISO.pl
#
# Prerequisite: Installed and configured data stream (see User Guide).
# -------------
#
# Usage: L4_C.pl [path] ...
# ------ For a complete list of options: L4_C.pl -h|-help
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# path                  string       IN  Absolute path of the target L4_C data
#                                        stream. For example:
#
#                                        /home/Operations/SPL4C/SPL4C_V05001
#
# L4_C.pl             integer      OUT  function return value:
#
#                                        0: success
#
#                                        1: usage error or runtime configuration
#                                           error.
#
#                                        2: Error returned from the
#                                           meteorology pre-processor method.
#
#                                        3: Error returned from the MODIS
#                                           pre-processor method.
#
#                                        4: Error occurred from the method
#                                           for executing the carbon model.
#
#                                        5: Error returned from the ISO
#                                           generation application.
#
#                                        6: Error occurred during file export.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           10/11/2014      J.Ardizzone  documented.
#           04/14/2015      J.Ardizzone  added codes to produce monitoring
#                                        files.
#******************************************************************************

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use Pod::Usage;
  use Time::Seconds;

  use CONFIG;
  use CLOCK;
  use FIND;
  use DTR;
  use TMGR;
  use DMGR;
  use SYSTEM;
  use LOG::Handler;
  use ERROR::Handler;
  use integer;

  my $rc;

# Retrieve run-time input
# parameters
# =======================

  CONFIG: my $config = getRunTimeConfig();

  ERROR_HANDLE:  my $this = "L4_C.pl";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $tmgr = TMGR->new($config->{L4_C_DAILY});
  $tmgr->isClockRunning() or exit 0;

  my $sys = SYSTEM->new($config->{L4_C_ROOT});
  $sys->isEnabled() or exit 0;

# Increment the system clock
# and retrieve the time to be
# processed.
# ===========================

  my $clock = CLOCK->new();
  my $today = $clock->strftime("%Y%m%d");

  exit 0 if $tmgr->{DATE} > $today;
  $tmgr->tickClock($config->{OP_RUN_FREQ});

  my $date = $config->{DATE};
  my $time = CLOCK->new(DATE=>$date,TIME=>0);

# Execute the meteorology
# preprocessor application.
# =========================

  my $input  = DMGR->new($config->{L4_C_IMPORT},%$config);
  my $output = DMGR->new($config->{L4_C_MET},%$config);

  $rc = runMet($time,$input,$output,$config);
  exit 2 if $eh->isError();
  $rc or exit 0;

  $input->{L4_C_MET} = $output->{L4_C_MET};

# Execute the MODIS
# preprocessor application.
# =========================

  my $time_8day = $time - ($time->yday % 8) * 86400;
  my $date_8day = $time_8day->strftime("%Y%m%d");
  $config->{L4C_MODEL_PERIOD} = $time_8day->yday / 8 + 1;

  my $dtr = $input->{MODIS};
  $input->{MODIS} = DTR->new($dtr->{DTR_FILE_ID},%$dtr,DATE=>$date_8day);
  $output = DMGR->new($config->{L4_C_MODIS},%$config,DATE=>$date_8day);

  $rc = runModis($time_8day,$input,$output,$config);
  exit 3 if $eh->isError();
  $rc or exit 0;

  $input->{L4_C_MODIS} = $output->{L4_C_MODIS};

# Execute the Model.
# ==================

  $output = DMGR->new($config->{L4_C_DAILY},%$config);

  $rc = runDaily($time,$input,$output,$config);
  exit 4 if $eh->isError();

  exit 0 if $rc == 2;
  $rc or exit 0;

# Generate the L4_C MDL granules.
# ===============================

  $rc = genISO($config);
  exit 5 if $eh->isError();
  $rc or exit 0;

# Export the HDF-5 files for
# customers.
# ==========================

  $rc = export($config);
  exit 6 if $eh->isError();
  $rc or exit 0;

# Generate files for monitoring
# Ignore error return codes. This
# step should not delay production.
# =================================

  monitor($config);

# Update the system clock and
# reset the system for processing
# a new date/time.
# ================================

  $eh->comment("L4_C is complete.");

  $tmgr->updateClock();
  $sys->closeProcess();

exit 0;

#******************************************************************************
sub runMet { CLOCK:  my $time = shift;
             DMGR:   my $input = shift;
             DMGR:   my $output = shift;
             CONFIG: my $config = shift;
#******************************************************************************
# English Name: Run Meteorology Preprocessor
# -------------
#
# Purpose: Executes all functions needed to complete the level-4 carbon
# -------- meteorological preprocessing step needed for executing the carbon
#          flux model.
#
# Language: Perl
# ---------
#
# See Also: CLOCK.pm, DMGR.pm, CONFIG.pm, ERROR::Handler.pm
# --------- 
#
# Notes: 1. This method resolves the following run-time input parameters for
# ------    the met preprocessor:
#
#           L4C_MET_INPUT_L3_SM_A_ROOTDIR
#           L4C_MET_INPUT_L3_SM_A_PATH
#           L4C_MET_INPUT_L4_SM_ROOTDIR
#           L4C_MET_INPUT_L4_SM_PATH
#           L4C_MET_INPUT_FP_ROOTDIR
#           L4C_MET_INPUT_FP_PATH
#           L4C_MET_INPUT_SOC_ROOTDIR
#           L4C_MET_INPUT_SOC_PATH
#           L4C_MET_INPUT_ANCILLARY_ROOTDIR
#           L4C_MET_INPUT_ANCILLARY_PATH
#           L4C_MET_OUTPUT_ROOTDIR
#           L4C_MET_OUTPUT_PATH
#
# Prerequisites: getRunTimeConfig()
# --------------
#
# Usage: $rc = runMet($time, $input, $output, $config)
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $time                  CLOCK       IN  CLOCK object set to the current
#                                        date/time.
#
# $input                  DMGR       IN  DMGR object describing all input
#                                        data for running the preprocessor.
#
# $output                 DMGR       IN  DMGR object describing all output
#                                        data to be produced by the 
#                                        preprocessor.
#
# $config               CONFIG       IN  CONFIG object describing the
#                                        system configuration and state. 
#
#  L4_C_MET                           *  Path to met preprocessor task
#                                        directory (e.g. .../run_MET).
#
# $rc                  integer      OUT  function return value:
#
#                                        0: waiting for input data
#                                        1: success
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler object
#
# $exception           integer      OUT  Thrown Exception:
#
#                                        0: no exception
#                                        1: met preprocessor is disabled
#                                        2: unable to create directory
#                                        3: error returned from met application
#                                        4: output verification failed
#                                        5: missing transaction file
#                                        6: error while renaming files
#                                        7: required input is missing
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           05/21/2014      J.Ardizzone  created.
#           04/14/2015      J.Ardizzone  Added error check for missing 
#                                        requirements (input data).
#******************************************************************************

  FUNCTION_NAME: my $this   = "runMet";
  ERROR_HANDLER: my $eh  = ERROR::Handler->new(\&error_handler,$this);

  my ($file, @files);
# my @inputs = (qw/LFO SPL4SMGP SPL3SMA SOC ANC/);
  my @inputs = (qw/LFO SPL4SMGP SOC ANC/);
  my $rip_path = File::Spec->catdir($config->{L4_C_MET},"rip");
  my $run_path = File::Spec->catdir($config->{L4_C_MET},"run");
  my $run_file = File::Spec->catfile($run_path,"l4c_MET.rip");

# Exit if this preprocessor has
# already completed or if it is disabled.
# =======================================

  my $sys = SYSTEM->new($config->{L4_C_MET});
  $sys->isEnabled() or return $eh->advisory(1);

  return 1 if ($output->stat("L4_C_MET"))[0];

  my $datetime = $time->datetime;
  $eh->comment( "Executing the MET Preprocessor: Time = $datetime");

# Retrieve all of the inputs. Exit if
# still waiting for one or more data types.
# =========================================

  my %options = %$config;
  $input->mexecute(\@inputs,\%options) or return 0;

# Required inputs

  foreach my $type (@inputs)
  { ($input->stat($type))[0] or $eh->error(7,TYPE=>$type) }

  return $eh->throw() if $eh->isError();

# Set Runtime Parameters
# ======================

  @files = $input->files("SPL3SMA");

  @files and do { 

    $options{L4C_MET_INPUT_L3_SM_A_ROOTDIR} = dirname $files[-1];
    $options{L4C_MET_INPUT_L3_SM_A_PATH} = basename $files[-1];

  };

  @files =  $input->files("SPL4SMGP");

  $options{L4C_MET_INPUT_L4_SM_ROOTDIR} = dirname $files[0];
  $options{L4C_MET_INPUT_L4_SM_PATH} = basenames(\@files);

  @files =  $input->files("LFO");

  $options{L4C_MET_INPUT_FP_ROOTDIR} = dirname $files[0];
  $options{L4C_MET_INPUT_FP_PATH} = basenames(\@files);

  @files =  $input->files("SOC");

  $options{L4C_MET_INPUT_SOC_ROOTDIR} = dirname $files[0];
  $options{L4C_MET_INPUT_SOC_PATH} = basename $files[0];

  @files =  $input->files("ANC",FILE_TYPE=>"SCIENCE");

  $options{L4C_MET_INPUT_ANCILLARY_ROOTDIR} = dirname $files[0];
  $options{L4C_MET_INPUT_ANCILLARY_PATH} = basename $files[0];

  my $dtr = $output->{L4_C_MET};
  $dtr or return $eh->error(5,TYPE=>"L4_C_MET");

  my ($out_file) = $dtr->files({FILE_TYPE=>"SCIENCE"});
  $out_file = $time->strftime($out_file);

  my $out_dir = dirname $out_file;
  makepath($out_dir) or return $eh->fatal(2,PATH=>$out_dir);

  $options{L4C_MET_OUTPUT_ROOTDIR} = $out_dir;
  $options{L4C_MET_OUTPUT_PATH} = basename $out_file;

  my ($log_file) = $dtr->files({FILE_TYPE=>"LOG"});
  $log_file = basename $time->strftime($log_file);

  my ($rip_file) = $dtr->files({FILE_TYPE=>"RIP"});
  $rip_file = basename $time->strftime($rip_file);

# Configure and execute the
# preprocessor.
# =========================

  makepath($run_path) or return $eh->fatal(2,PATH=>$run_path);
  $config->jobConfig($rip_path,$run_path,%options);

  my $pwd = `pwd`; chomp $pwd;
  chdir $run_path;

  system "l4c_MET.ex l4c_MET.rip" and return $eh->error(3);

# Rename files that are missing a date/time
# stamp for LOM storage.
# =========================================

  move ("SMAP_L4_C_MET.log", $log_file) or return $eh->error(6,FILE=>$log_file);
  move ("l4c_MET.rip", $rip_file) or return $eh->error(6,FILE=>$rip_file);

# Verify outputs and exit.
# ========================

  $output->execute("L4_C_MET");
  ($output->stat("L4_C_MET"))[0] or return $eh->error(4);

  chdir $pwd;

  $eh->comment("l4c_MET preprocessing is complete");

  return 1;

}

#******************************************************************************
sub runModis { CLOCK:  my $time = shift;
               DMGR:   my $input = shift;
               DMGR:   my $output = shift;
               CONFIG: my $config = shift;
#******************************************************************************
# English Name: Run MODIS Preprocessor
# -------------
#
# Purpose: Executes all functions needed to complete the level-4 carbon
# -------- MODIS preprocessing step needed for executing the carbon
#          flux model.
#
# Language: Perl
# ---------
#
# Notes: 1. This method renames the log file and runtime input file created
# ------    as part of the modis preprocessing. It assumes values for the
#           default filenames. Ths method will fail if the default conventions
#           are changed.
#
# See Also: CLOCK.pm, DMGR.pm, CONFIG.pm, ERROR::Handler.pm
# --------- 
#
# Prerequisites: getRunTimeConfig()
# --------------
#
# Usage: $rc = runModis($time, $input, $output, $config)
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $time                  CLOCK       IN  CLOCK object set to the date/time
#                                        of the MODIS 8-day period.
#
# $input                  DMGR       IN  DMGR object describing all input
#                                        data for running the preprocessor.
#
# $output                 DMGR       IN  DMGR object describing all output
#                                        data to be produced by the 
#                                        preprocessor.
#
# $config               CONFIG    INOUT  CONFIG object describing the system
#                                        configuration and state. Acquired 
#                                        data transaction and preprocessor
#                                        specific parameters are returned as
#                                        hash entries. Returned parmeters are:
#
#                                        L4C_GPP_SOURCE                       
#                                        L4C_INPUT_MODIS_ROOTDIR
#                                        L4C_INPUT_MODIS_FPAR_GRANULE
#                                        L4C_INPUT_MODIS_ANCILLARY_ROOTDIR
#                                        L4C_INPUT_MODIS_ANCILLARY_PATH
#                                        L4C_OUTPUT_MODIS_ROOTDIR
#                                        L4C_OUTPUT_MODIS_PATH
#                                        L4C_OUTPUT_LOG_ROOTDIR
#
# $rc                  integer      OUT  function return value:
#
#                                        0: waiting for input data
#                                        1: success
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler object.
#
# $exception           integer      OUT  Thrown Exception:
#
#                                        0: no exception
#                                        1: modis preprocessor is disabled
#                                        2: unable to create directory
#                                        3: error returned from modis processor
#                                        4: output verification failed
#                                        5: missing transaction file
#                                        6: required inputs are missing
#                                        7: modis data is unavailable
#                                        8: error while renaming files
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           05/21/2014      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this = "runModis";
  ERROR_HANDLER: my $eh   = ERROR::Handler->new(\&error_handler,$this);

  my @inputs = (qw/MODIS ANC/);
  my $date = $time->strftime("%Y%m%d");
  my $rip_path = File::Spec->catdir($config->{L4_C_MODIS},"rip");
  my $run_path = File::Spec->catdir($config->{L4_C_MODIS},"run");
  my $run_file = File::Spec->catfile($run_path,"l4c_MODIS.rip");

# Exit if this preprocessor has
# already completed or if it is disabled.
# =======================================

  my $sys = SYSTEM->new($config->{L4_C_MODIS});
  $sys->isEnabled() or return $eh->advisory(1);

  $config->{L4C_GPP_SOURCE} = "FPAR";
  return 1 if ($output->stat("L4_C_MODIS"))[0];

  my $datetime = $time->datetime;
  $eh->comment( "Executing the MODIS Preprocessor: Time = $datetime");

# Retrieve all of the inputs. Exit if
# still waiting for one or more data types.
# =========================================

  my %options = %$config;
  $input->mexecute(\@inputs,\%options) or return 0;

  ($input->stat("ANC"))[0] or return $eh->error(6,TYPE=>"ANC");

  $config->{L4C_GPP_SOURCE} = $config->{OP_CLIM} ? "CLIM" : "FPAR";
  $options{L4C_GPP_SOURCE}  = $config->{L4C_GPP_SOURCE};

  if ($config->{L4C_GPP_SOURCE} eq "CLIM") {
    $eh->warning(7);
    return 1;
  }

  my @result = $input->stat("MODIS");

  $result[0] or do {
      $eh->warning(9,COUNT=>$result[4]);
  };

# Set Runtime Parameters
# ======================

  my @files =  $input->files("MODIS");

  $options{L4C_INPUT_MODIS_ROOTDIR} = dirname $files[0];
  $options{L4C_INPUT_MODIS_FPAR_GRANULE} = basenames(\@files);

  my ($anc_file) =  $input->files("ANC",FILE_TYPE=>"SCIENCE");

  $options{L4C_INPUT_MODIS_ANCILLARY_ROOTDIR} = dirname $anc_file;
  $options{L4C_INPUT_MODIS_ANCILLARY_PATH} = basename $anc_file;

  my $dtr = $output->{L4_C_MODIS};
  $dtr or return $eh->error(5,TYPE=>"L4_C_MODIS");

  my ($out_file) = $dtr->files({FILE_TYPE=>"SCIENCE"});
  $out_file = $time->strftime($out_file);

  my $out_dir = dirname $out_file;
  makepath($out_dir) or return $eh->fatal(2,PATH=>$out_dir);

  my ($log_file) = $dtr->files({FILE_TYPE=>"LOG"});
  $log_file = basename $time->strftime($log_file);

  my ($rip_file) = $dtr->files({FILE_TYPE=>"RIP"});
  $rip_file = basename $time->strftime($rip_file);

  $options{L4C_OUTPUT_MODIS_ROOTDIR} = $out_dir;
  $options{L4C_OUTPUT_MODIS_PATH} = basename $out_file;
  $options{L4C_OUTPUT_LOG_ROOTDIR} = $run_path;

# Configure and execute the
# preprocessor.
# =========================

  makepath($run_path) or return $eh->fatal(2,PATH=>$run_path);
  $config->jobConfig($rip_path,$run_path,DATE=>$date,%options);

  my $pwd = `pwd`; chomp $pwd;
  chdir $run_path;

  my $now = CLOCK->new();
  system "l4c_MODIS.ex l4c_MODIS.rip" and return $eh->error(3);

# Rename files that are missing a date/time
# stamp for LOM storage.
# =========================================

  move ("l4c_MODIS.log", $log_file) or return $eh->error(8,FILE=>$log_file);
  move ("l4c_MODIS.rip", $rip_file) or return $eh->error(8,FILE=>$rip_file);

# Verify outputs and exit.
# ========================

  $output->execute("L4_C_MODIS");
  ($output->stat("L4_C_MODIS"))[0] or return $eh->error(4);

  chdir $pwd;

  $eh->comment("l4c_MODIS preprocessing is complete");

  return 1;

}

#******************************************************************************
sub runDaily { CLOCK:  my $time = shift;
               DMGR:   my $input = shift;
               DMGR:   my $output = shift;
               CONFIG: my $config = shift;
#******************************************************************************
# English Name: Run Daily Carbon Model
# -------------
#
# Purpose: Executes the carbon flux model (DAILY).
# -------- 
#
# Language: Perl
# ---------
#
# See Also: CLOCK.pm, DMGR.pm, CONFIG.pm, ERROR::Handler.pm
# --------- 
#
# Prerequisites: getRunTimeConfig()
# --------------
#
# Usage: $rc = runDAILY($time, $input, $output, $config)
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $time              CLOCK ref       IN  CLOCK object reference set to the
#                                        current date/time.
#
# $input              DMGR ref       IN  DMGR object reference describing all
#                                        input data for running the model.
#
# $output             DMGR ref       IN  DMGR object reference describing all
#                                        output data to be produced by the 
#                                        carbon model.
#
# $config           CONFIG ref       IN  CONFIG object reference describing the
#                                        system configuration and state. 
#
# $rc                  integer      OUT  function return value:
#
#                                        0: waiting for input data
#                                        1: success
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler referent.
#
# $exception           integer      OUT  Thrown Exception:
#
#                                        1: Carbon model is disabled
#                                        2: Unable to create directory
#                                        3: l4c_daily exited with errors
#                                        4: Expected output is incomplete
#                                           or missing
#                                        5: Missing data transaction for type
#                                          
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           05/26/2014      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this   = "runDaily";
  ERROR_HANDLER: my $eh  = ERROR::Handler->new(\&error_handler,$this);

  my $dtr;
  my @inputs = (qw/L4_C_MET L4_C_MODIS FPAR_CLIMATOLOGY SOC ANC/);
  my $rip_path = File::Spec->catdir($config->{L4_C_DAILY},"rip");
  my $run_path = File::Spec->catdir($config->{L4_C_DAILY},"run");
  my $run_file = File::Spec->catfile($run_path,"l4c_DAILY.rip");

# Exit if the model has already
# completed or if it is disabled.
# ===============================

  my $sys = SYSTEM->new($config->{L4_C_DAILY});
  $sys->isEnabled() or return $eh->advisory(1);

  return 2 if ($output->stat("L4_C_DAILY"))[0];

  my $datetime = $time->datetime;
  $eh->comment( "Executing the Carbon Model: " .
                      "Collection = MDL, " .
                      "Time = $datetime");

# Retrieve all of the inputs
# Exit if still waiting for
# one or more data types.
# ==========================

  my %options = %$config;
  $input->mexecute(\@inputs,\%options) or return 0;

# Copy the SOC restart file and save under
# the current date/time.
# ========================================

  my ($soc_file) = $input->files("SOC",FILE_TYPE=>"SCIENCE");

  $dtr = $output->{L4_C_DAILY};
  $dtr or return $eh->error(5,TYPE=>"L4_C_DAILY");

  my ($restart_file) = $dtr->files({FILE_TYPE=>"RESTART"});
  $restart_file = $time->strftime($restart_file);

  my $restart_path = dirname $restart_file;
  makepath($restart_path) or return $eh->fatal(2,PATH=>$restart_path);
  copy ($soc_file, $restart_file) or return $eh->error(6);

# Set Runtime Parameters
# ======================

  my ($mod_file) = $input->files("L4_C_MODIS",FILE_TYPE=>"SCIENCE");
  $options{L4C_INPUT_FPAR_8DAY_PATH} = $mod_file;

  my ($clim_file) = $input->files("FPAR_CLIMATOLOGY");
  $options{L4C_INPUT_FPAR_CLIMATOLOGY_PATH} = $clim_file;

  $options{L4C_INPUT_FPAR_8DAY_PATH} = $options{L4C_INPUT_FPAR_8DAY_PATH} ||
                                   $options{L4C_INPUT_FPAR_CLIMATOLOGY_PATH};

  my ($met_file) = $input->files("L4_C_MET",FILE_TYPE=>"SCIENCE");
  $options{L4C_INPUT_MET_PATH} = $met_file;

  my ($soc_file) =  $input->files("SOC");
  $options{L4C_INPUT_SOC_PATH} = $soc_file;

  my ($anc_file) = $input->files("ANC",FILE_TYPE=>"SCIENCE");
  $options{L4C_INPUT_ANCILLARY_PATH} = $anc_file;
  $options{L4C_INPUT_ROOTDIR} = dirname $anc_file;

  my ($bplut_file) = $input->files("ANC",FILE_TYPE=>"BPLUT");
  $options{L4C_INPUT_BPLUT_PATH} = $bplut_file;

  $dtr = $output->{L4_C_DAILY};
  $dtr or return $eh->error(5,TYPE=>"L4_C_DAILY");

  my ($out_file) = $dtr->files({FILE_TYPE=>"SCIENCE"});
  $out_file = $time->strftime($out_file);

  my $out_dir = dirname $out_file;
  makepath($out_dir) or return $eh->fatal(2,PATH=>$out_dir);

  my ($log_file) = $dtr->files({FILE_TYPE=>"LOG"});
  $log_file = basename $time->strftime($log_file);

  my ($rip_file) = $dtr->files({FILE_TYPE=>"RIP"});
  $rip_file = basename $time->strftime($rip_file);

  $options{L4C_OUTPUT_ROOTDIR} = $out_dir;
  $options{L4C_OUTPUT_DAILY_PATH} = $out_file;
  $options{L4C_OUTPUT_DAILY_LOG_ROOTDIR} = $run_path;

# Configure and execute the
# model
# =========================

  makepath($run_path) or return $eh->fatal(2,PATH=>$run_path);
  $config->jobConfig($rip_path,$run_path,%options);

  $eh->comment("Executing l4c_DAILY");

  my $pwd = `pwd`; chomp $pwd;
  chdir $run_path;

  system "l4c_DAILY.ex l4c_DAILY.rip" and return $eh->error(3);

# Rename files that are missing a date/time
# stamp for LOM storage.
# =========================================

  move ("SMAP_L4_C_MDL.log", $log_file) or return $eh->error(7,FILE=>$log_file);
  move ("l4c_DAILY.rip", $rip_file) or return $eh->error(7,FILE=>$rip_file);

# Verify outputs and exit.
# ========================

  $output->execute("L4_C_DAILY");
  ($output->stat("L4_C_DAILY"))[0] or return $eh->error(4);

  chdir $pwd;

  $eh->comment("l4c_DAILY processing is complete");

  return 1;

}

#******************************************************************************
sub genISO { CONFIG: my $config = shift;
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

  return 1 if $config->{OP_NO_HDF};

  my $this = "genISO";
  my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $pc         = $config->{PRODUCT_COUNTER};
  my $dictionary = $config->{DICTIONARY};

  system( "L4_C_genISO.pl -pc $pc " .
          "$config->{L4_C_ROOT} " .
          "$config->{DATE} 0 " .
          hash2opt(%$dictionary)
        ) and return $eh->error(1);

  return 1;

}

#******************************************************************************
sub monitor { CONFIG: my $config = shift;
#******************************************************************************
# English Name: Monitor
# -------------
#
# Purpose: Generates files used for monitring data streams for quality
# -------- control and assurance.
#
# Language: Perl
# ---------
#
# See Also: L4.pl, CONFIG.pm, ERROR::Handler.pm, &error_handler
# ---------
#
# Notes: 1. Directory paths must be defined for each of the monitoring data
# ------    types. The year, month and day portion of the directory are
#           automatically appended by this method. No moniotring files will
#           generated if the associated directory path/ID is undefined. There
#           are two monitoring types:
#
#           a. BROWSE - browse imagery
#           b. QuADS - Quality Assurance of Data Sets (images)
#
# Usage: $rc = monitor($config)
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $config               CONFIG       IN  CONFIG object describing the
#                                        system configuration and state.
#
#  {L4_C_BROWSE_directoryID}          *  base directory path for storing
#                                        browse imagery (see note-1).
#
#  {L4_C_QUADS_directoryID}           *  base directory path for storing
#                                        QuADS imagery (see note-1).
#
# $rc                  integer      OUT  function return value:
#
#                                        0: invalid or incorrect usage
#                                        1: success
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler referent.
#
# $exception           integer      OUT  Thrown Exception:
#
#                                        1: Error generating monitoring files.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           04/13/2015      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this = "monitor";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  $config->{OP_NO_HDF} and return 1;

  $eh->comment("Generating data for monitoring: Date = $config->{DATE}");

  my $date        = $config->{DATE};
  my $stream_type = $config->{OP_STREAM_TYPE};
  my $stream_name = $config->{OP_STREAM_NAME};

  my ($outdir, $output);
  my $pc = $config->{PRODUCT_COUNTER};

# Generate Browse Imagery
# =======================

  $outdir = $config->{L4_C_BROWSE_directoryID};
  $output = File::Spec->catdir($outdir, "Y%Y", "M%m", "D%d");

  system("L4.pl -browse -st $stream_type -sn $stream_name -date $date " .
      "-pc $pc -auto") and return $eh->error(1,TYPE=>"browse") if $outdir;

# Generate QuADS Imagery
# ======================

  $outdir = $config->{L4_C_QUADS_directoryID};
  $output = File::Spec->catdir($outdir, "Y%Y", "M%m", "D%d");

  system("L4.pl -quads -st $stream_type -sn $stream_name -date $date " .
      "-pc $pc -auto") and return $eh->error(1,TYPE=>"QuADS") if $outdir;

  return 1;
}

sub export { CONFIG: my $config = shift;

  $config->{OP_EXPORT} or  return 1;
  $config->{OP_NO_HDF} and return 1;

  FUNCTION_NAME: my $this = "export";
  ERROR_HANDLER: my $eh   = ERROR::Handler->new(\&error_handler,$this);

  my $export     = DMGR->new($config->{L4_C_EXPORT},%$config);
  my $dictionary = $config->{DICTIONARY};

  foreach my $key (keys %$export) {

    ($export->stat($key))[0] and next;

    my $dtr = $export->{$key};

    my $sys = SYSTEM->new($dtr->{DTR_DIR_ID},%$config);
    $sys->isEnabled() or do {$eh->advisory(1,TYPE=>$key), next };

    $eh->comment("Exporting " . $key);

    my $hasValue = exists $dictionary->{PDR_EXPORT_DIR};

    my $export_dir = $dictionary->{PDR_EXPORT_DIR};
    $dictionary->{PDR_EXPORT_DIR} = $dtr->{PDR_EXPORT_DIR} if ! $hasValue;
    $dictionary->{PDR_EXPORT_DIR} .= "_hold" if ! $config->{OP_NO_HOLD};

    $export->execute($key);

    $dictionary->{PDR_EXPORT_DIR} = $export_dir;
    $hasValue or delete $dictionary->{PDR_EXPORT_DIR};

    ($export->stat($key))[0] or $eh->error(2,TYPE=>$key);

  }

  return $eh->throw() if $eh->isError();

  return 1;
}

sub makepath { my $dir = shift; return 1 if -d $dir; mkpath $dir or return 0 }

sub basenames { my $files = shift; 

  my @files = map { basename $_ } @$files;
  return \@files;
}

sub hash2opt { my %hash = @_;

  my $opts = undef;

  foreach my $key (keys %hash) {

    $opts = $opts . "-define " . $key . '="' . $hash{$key} . '" ';

  }

  return $opts;

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
#           11/01/2013      J.Ardizzone  created.
#******************************************************************************
{

# Retrieve command-line
# information.
# =====================

  my %option = getArgs();

  my $dictionary      = $option{OP_DEFINE};
  $option{DICTIONARY} = $dictionary;

  $option{PRODUCT_COUNTER} = $option{OP_PROD_COUNTER} // 1;
  $option{PRODUCT_COUNTER} = substr($option{PRODUCT_COUNTER}+1000, 1);

  $dictionary->{WAIT_TIME} = 0 if $option{OP_FORCE};

  my $path = $option{OP_STREAM_DIR};

# Derive key parameters
# =====================

  $option{L4_STREAM}       = basename $path;
  $option{JOBNAME}         = $option{L4_STREAM};

  $option{L4_C_ROOT}   = $path;
  $option{L4_C_PATH}   = File::Spec->catdir($option{L4_C_ROOT}, "bin");
  $option{L4_C_IMPORT} = File::Spec->catdir($option{L4_C_ROOT}, "import");
  $option{L4_C_EXPORT} = File::Spec->catdir($option{L4_C_ROOT}, "export");
  $option{L4_C_MET}    = File::Spec->catdir($option{L4_C_ROOT}, "run_MET");
  $option{L4_C_MODIS}  = File::Spec->catdir($option{L4_C_ROOT}, "run_MODIS");
  $option{L4_C_DAILY}  = File::Spec->catdir($option{L4_C_ROOT}, "run_DAILY");

  my $log_file         = File::Spec->catdir($option{L4_C_ROOT}, "log");
  $ENV{L4_LOG_FILE}    = $ENV{L4_LOG_FILE} // $log_file;
  $option{L4_LOG_FILE} = $ENV{L4_LOG_FILE};

  $ENV{PATH}= join ":", $option{L4_C_PATH}, $ENV{PATH};

# Retrieve the date/time
# from the system clock.
# ======================

  my $tmgr = TMGR->new($option{L4_C_DAILY});

  $option{DATE} = $tmgr->{DATE};
  $option{TIME} = $tmgr->{TIME};

# Configure the log handler
# =========================

  my $lh = LOG::Handler->new(SYSTEM=>"L4_C_SPS",
                             APPLICATION=>"L4_C",
                             DATE=>$option{DATE},
                             TIME=>$option{TIME});

# Return the run-time configuration
# =================================

  my $config = CONFIG->new($option{L4_C_DAILY},%option);

  return $config;

}

#******************************************************************************
sub getArgs
#******************************************************************************
# English Name: Get Arguments
# -------------
#
# Purpose: Retrieves command-line arguments and options. 
# --------
#
# Language: Perl
# ---------
#
# Notes: 1. The environment variable, SMAP_OPS_DIR, must be defined if the
# ------    stream to be processed is identified by its type and name only
#           (i.e. the full pathname is not specified as a command-line
#           argument).
#
# See Also: getRunTimeConfig()
# ---------
#
# Prerequisites: $ENV{SMAP_OPS_DIR}
# --------------
#
# Usage: %options = getArgs()
# ------ $rc = $eh->isError()
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# %options                HASH      OUT  Hash table of returned command-line
#                                        options.
#
#  {OP_FORCE}          boolean      OUT  Forces processing to continue even
#                                        if some secondard inputs are
#                                        incomplete or unavailable.
#  
#  {OP_PROD_COUNTER}   integer      OUT  Product counter expressed as a 3-digit
#                                        integer (default: 001).
#
#  {OP_RUN_FREQ}       integer      OUT  Run frequency in seconds. This is the
#                                        interval of time between invocation
#                                        of this application (default: 3600).
#
#  {OP_STREAM_NAME}     string      OUT  Name of the stream to be processed
#                                        (e.g. SPL4C_V05005).
#  
#  {OP_STREAM_TYPE}     string      OUT  Name of the stream type to be
#                                        processed (e.g. SPL4C).
#
#  {OP_RESUME}         boolean      OUT  Resume post-model processing. This
#                                        option is useful when errors
#                                        occur in post-processing and cause
#                                        a premature exit.
#
#  {OP_STREAM_DIR}      string      OUT  Pathname of the stream directory. This
#                                        can be specified on the command-line
#                                        or can be derived from the stream
#                                        type and name options in combination
#                                        with the environment variable:
#
#                                        SMAP_OPS_DIR
#
# $eh           ERROR::Handler      OUT  Error handler object.
#
# $exception           integer      OUT  Thrown Exception:
#
#                                        1: Unable to identify stream directory
#                                        2: Stream directory does not exist
#                                        3: Help on usage requested
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           11/03/2014      J.Ardizzone  created.
#******************************************************************************
{
  my %options = ();
  my %define  = ();
  my ($export, $force, $help, $idle, $man, $noHDF, $noHold, $clim, $pc);
  my ($resume, $run_freq, $stream_name, $stream_type);

# Retrieve command-line options
# =============================

  GetOptions ("help|h" => \$help,
              "man" => \$man,
              "clim" => \$clim,
              "define=s" => \%define,
              "export" => \$export,
              "force|f" => \$force,
              "idle=i" => \$idle,
              "noHDF" => \$noHDF,
              "noHold" => \$noHold,
              "pc=i" => \$pc,
              "resume" => \$resume,
              "run_freq|rf=i" => \$run_freq,
              "stream_name|sn=s" => \$stream_name,
              "stream_type|st=s" => \$stream_type);

  my @path = File::Spec->splitdir( $ARGV[0] );

  $options{OP_HELP}         = $help;
  $options{OP_DEFINE}       = \%define;
  $options{OP_EXPORT}       = $export;
  $options{OP_FORCE}        = $force;
  $options{OP_IDLE}         = $idle || 0;
  $options{OP_NO_HDF}       = $noHDF;
  $options{OP_NO_HOLD}      = $noHold;
  $options{OP_CLIM}         = $clim;
  $options{OP_PROD_COUNTER} = $pc // "001";
  $options{OP_RUN_FREQ}     = $run_freq || 3600;
  $options{OP_RESUME}       = $resume;

  $options{OP_STREAM_NAME}  = $stream_name // pop @path;
  $options{OP_STREAM_TYPE}  = $stream_type // pop @path;
  $options{OP_STREAM_DIR}   = $ARGV[0] // File::Spec->catdir($ENV{SMAP_OPS_DIR},
                                          $options{OP_STREAM_TYPE},
                                          $options{OP_STREAM_NAME});

  $help and pod2usage(1);
  $man  and pod2usage(-verbose => 2);

  -d $options{OP_STREAM_DIR} or do {
    print "The stream directory does not exist. Please check arguments.\n";
    exit 1;
  };

  return %options;

}
#******************************************************************************
sub error_handler { my $error_handle = shift;
                    my $error_code   = shift;
                    my %options      = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Error Handler
# -------------
#
# Purpose: Reports exceptions for the invoking method. This error handler
# -------- is a catalog of registed exceptions for all methods contained in this
#          application.
#
# Language: Perl
# ---------
#
# See Also: ERROR::Handler.pm, LOG::Handler.pm
# --------- 
#
# Notes: 1. Developers must register new or changed exceptions by adding or
# ------    updating the error handler blocks contained in this method.
#
#        2. This method is intended to be invoked by the ERROR::Handler
#           module. Please see ERROR::Handler.pm for more information.
#
# Usage: $eh = ERROR::Handler->new(\&error_handler,$this);
# ------ $eh->$type($error_handle, $error_code, %options);
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $eh           ERROR::Handler       IN  Error Handler object instantiated by
#                                        the invoking method.
#
# \&error_handler         CODE       IN  Reference to this method.
#
# $type                 string       IN  error method type (see note-2).
#
# $error_handle         string       IN  name of invoking method.
#
# $error_code          integer       IN  exception identifier.
#
# %options                hash       IN  optional arguments used to resolve
#                                        an exception message.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           08/27/2014      J.Ardizzone  created.
#******************************************************************************

  my $type   = $options{ERROR_TYPE};
  my $lh     = LOG::Handler->new(HANDLE=>$error_handle);

  defined $options{ERROR_ELAPSED_TIME} and do {

    my $seconds = $options{ERROR_ELAPSED_TIME};
    $lh->comment(0,"Exiting $error_handle : [${seconds}s]");
    return;

  };

  $options{ERROR_TRACEBACK} and do {

    $lh->error($error_code,"Traceback: $error_handle() encountered an error.");
    return;

  };

  $error_code or return $lh->comment(0,$options{COMMENT});

# runMet() error handle
# =====================

  $error_handle eq "runMet" and do {

    $error_code == 1 and do {
      $lh->$type(1,"MET preprocessor (l4c_MET) is disabled.");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Unable to create directory: \"$options{PATH}\".");
      return;
    };

    $error_code == 3 and do {
      $lh->$type(3,"l4c_MET exited with errors.");
      return;
    };

    $error_code == 4 and do {
      $lh->$type(4,"Expected output is incomplete or missing.");
      return;
    };

    $error_code == 5 and do {
      $lh->$type(5,"Missing data transaction for type: \"$options{TYPE}\".");
      return;
    };

    $error_code == 6 and do {
      $lh->$type(6,"Unable to create file: \"$options{FILE}\".");
      return;
    };

    $error_code == 7 and do {
      $lh->$type(7,"Required input data type is missing: \"$options{TYPE}\".");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# runModis() error handle
# =======================

  $error_handle eq "runModis" and do {

    $error_code == 1 and do {
      $lh->$type(1,"MODIS preprocessor (l4c_MODIS) is disabled.");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Unable to create directory: \"$options{PATH}\".");
      return;
    };

    $error_code == 3 and do {
      $lh->$type(3,"l4c_MODIS exited with errors.");
      return;
    };

    $error_code == 4 and do {
      $lh->$type(4,"Expected output is incomplete or missing.");
      return;
    };

    $error_code == 5 and do {
      $lh->$type(5,"Missing data transaction for type: " .  
                   "\"$options{TYPE}\".");
      return;
    };

    $error_code == 6 and do {
      $lh->$type(6,"Required data is missing: \"$options{TYPE}\".");
      return;
    };

    $error_code == 7 and do {
      $lh->$type(7,"Using climatology.");
      return;
    };

    $error_code == 8 and do {
      $lh->$type(8,"Unable to create file: \"$options{FILE}\".");
      return;
    };

    $error_code == 9 and do {
      $lh->$type(9,"Number of MODIS files below expected value: " .
                   "$options{COUNT}");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# runDaily() error handle
# =======================
   
  $error_handle eq "runDaily" and do {
     
    $error_code == 1 and do {
      $lh->$type(1,"Carbon model (l4c_DAILY) is disabled.");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Unable to create directory: \"$options{PATH}\".");
      return;
    };

    $error_code == 3 and do {
      $lh->$type(3,"l4c_DAILY exited with errors.");
      return;
    };

    $error_code == 4 and do {
      $lh->$type(4,"Expected output is incomplete or missing.");
      return;
    };

    $error_code == 5 and do {
      $lh->$type(5,"Missing data transaction for type: " .
                   "\"$options{TYPE}\".");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# genISO() error handle
# =======================

  $error_handle eq "genISO" and do {

    $error_code == 1 and do {
      $lh->$type(1,"L4_C_genISO.pl exited with errors.");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# monitor() error handle
# ======================

  $error_handle eq "monitor" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Error generating $options{TYPE} files for monitoring");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# export() error handle
# =======================

  $error_handle eq "export" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Exporting is disabled for type: \"$options{TYPE}\".");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Export failed for type: \"$options{TYPE}\".");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

  print STDERR "No error handle for \"$error_handle\"\n";

  return;

}
