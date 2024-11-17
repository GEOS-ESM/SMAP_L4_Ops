#!/usr/bin/perl

#******************************************************************************
# English Name: Level-4 Soil Moisture - Generate ISO
# -------------
#
# The text following this prolog contains documentation for this application
# using Perl POD markup language. Formatted documentation can be obtained
# using the following commands:
#
#        perldoc L4_SM.pl
#        L4_SM.pl -h|-help
#        L4_SM.pl -man
#
#******************************************************************************

=head1 NAME

L4_SM.pl - Executes a SMAP Level-4 soil moisture (L4_SM) data stream.

=head1 SYNOPSIS

L4_SM.pl [-h|help] [-man] [-export] [-f|force] [-noHDF] [-noHold] [-pc counter] [-rf|run_freq interval] [-sn|stream_name name] [-st|stream_type type]

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

=item -noHDF

Turn HDF processing off. Level-4 granules will not be automatically generated. Use L4_SM_genISO.pl to generate granules as a separate task.

=item -noPost

Turn post-processing off. No post-processing will be performed. HDF, percentile and monitoring files will not be created. Exporting is also disabled.

=item -noHold

Do not hold PDRs when -export is specified. Use this option to send PDRs directly to the polled directory for immediate transmission to clients without inspection.

=item -pc counter

Set the product counter. I<counter> is an integer between 1 and 999. The default
value is 1. Please reference L4_SM_genISO.pl for more information.

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

Executes a SMAP Level-4 soil moisture (L4_SM) data stream. This is the main driver for end-to-end processing including data acquisition, model execution, HDF-5 generation and export of final SMAP Level-4 soil moisture products.

=head1 NOTES

=over 4

=item 1.

This application requires a configured L4_SM data stream. The following documentation is available:

 "SMAP Level-4 SPS User Guide"
 "SMAP Level-4 Soil Moisture SPS User Guide"

=item 2.

This application must be periodically invoked to complete processing. It is typically managed as a CRON job or background daemon process. It can also run under the Dependency-Based Operational System Scheduler (D-BOSS).

=item 3.

The L4 SPS keeps a record of completed tasks. This application will simply exit if the system records indicate that processing was completed for the specified date/time. Use the L4 SPS dashboard command (L4.pl) for rewinding or reprocessing. Please reference the L4 SPS documentation for more information.

=back

=head1 SEE ALSO

L4_SM_genISO.pl(1), L4.pl(1)

=head1 AUTHORS

Joseph V. Ardizzone - NASA Goddard Space Flight Center, Global Modeling and Assimilation Office (GMAO)

=head1 COPYRIGHT

This software is the property of the National Aeronautics and Space
Administration (NASA) and is subject to the regulations contained in the NASA
Procedural Requirements document NPR 2210.1C managed by the Office of the Chief
Technologist.

=cut


#******************************************************************************
# English Name: Level-4 Soil Moisture SPS - Main Driver
# -------------
#
# Purpose: Executes a SMAP Level-4 soil moisture (SPL4SM) data stream. This is
# -------- the main driver for end-to-end processing including data acquisition,
#          model execution, HDF generation, ISO metadata and export of final
#          SMAP Level-4 soil moisture (L4_SM) products.
#
# Language: Perl
# ---------
#
# Notes: 1. This application requires a configured L4_SM data stream. The
# ------    following documentation is available:
#
#           "SMAP Level-4 SPS User's Guide"
#           "SMAP Level-4 Soil Moisture SPS User's Guide"
#
#        2. This application must be periodically invoked to complete 
#           processing. It is typically managed as a CRON job or background
#           daemon process (see L4_SM.d). It can also run under the 
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
#           L4_SM_genISO.pl
#
# Prerequisite: Installed and configured data stream (see User's Guide).
# -------------
#
# Usage: L4_SM.pl [path] [-pc product counter]
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# path                  string       IN  Absolute path of the target L4_SM data
#                                        stream. For example:
#
#                                        /home/Operations/SPL4SM/SPL4SM_V05001
#
# product counter      integer   OPT,IN  Optional 3-digit product counter
#                                        (default: 001)
#
# L4_SM.pl             integer      OUT  function return value:
#
#                                        0: success
#
#                                        1: usage error or runtime configuration
#                                           error.
#
#                                        2: Error returned from runModel()
#                                           L4_C granules.
#
#                                        3: Error returned from the LDAS
#                                           post-processor - genPCTL. This
#                                           often occurs if old files are not
#                                           deleted before reprocessing.
#
#                                        4: Error returned from the ISO
#                                           generation application. 
#
#                                        5: Error occurred during file export.
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           10/11/2014      J.Ardizzone  documented.
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
  use SMAPLIST;
  use LOG::Handler;
  use ERROR::Handler;

  my $rc;

# Retrieve run-time input
# parameters
# =======================

  CONFIG: my $config = getRunTimeConfig();

  ERROR_HANDLE:  my $this = "L4_SM.pl";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $tmgr = TMGR->new($config->{L4_SM_MODEL});
  $tmgr->isClockRunning() or exit 0;

  my $sys = SYSTEM->new($config->{L4_SM_ROOT});
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

# Load input and output data
# transactions.
# ==========================

  my $input  = DMGR->new($config->{L4_SM_IMPORT},%$config);
  my $output = DMGR->new($config->{L4_SM_MODEL},%$config);

# Execute the LDAS
# ================

  $rc = runModel($time,$input,$output,$config);
  exit 2 if $eh->isError();

  exit 0 if $rc == 2;
  $rc or exit 0;

# Check the LDAS log file for
# warnings. Log warning if present.
# =================================

  $rc = checkLog($time, $output, $config);

# Generate the percentile fields
# ==============================

  $rc = genPCTL($config);
  exit 3 if $eh->isError();

# Convert the LDAS output to HDF-5
# ================================

  genISO($config);
  exit 4 if $eh->isError();

# Generate files for monitoring
# Ignore error return codes. This
# step should not delay production.
# =================================

  monitor($config);

# Export the HDF-5 files for
# customers.
# ==========================

  export($config);
  exit 5 if $eh->isError();

# Update the system clock and
# reset the system for processing
# a new date/time.
# ================================

  $sys = SYSTEM->new($config->{L4_SM_MODEL});
  exit 0 if $sys->isReady();

  $eh->comment("L4_SM is complete.");

  $tmgr->updateClock();
  $sys->closeProcess();

exit 0;


#******************************************************************************
sub runModel { CLOCK:  my $time = shift;
               DMGR:   my $input = shift;
               DMGR:   my $output = shift;
               CONFIG: my $config = shift;
#******************************************************************************
# English Name: Run Model (LDASsa)
# -------------
#
# Purpose: Executes the Land Data Assimilation System (LDAS). This method
# -------- performs all function for acquiring input data, submitting the
#          LDAS batch job and verifying completion.
#
# Language: Perl
# ---------
#
# See Also: DMGR.pm, CONFIG.pm, DTR.pm, CLOCK.pm, ERROR::Handler.pm
# --------- SYSTEM.pm, getRunTimeConfig()
#
# Prerequisite: getRunTimeConfig()
# -------------
#
# Usage: $rc = runModel($time,$input,$output,$config);
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $time                  CLOCK       IN  CLOCK object reference set to the
#                                        date/time to be processed.
#
# $input                  DMGR       IN  DMGR object reference describing all
#                                        input data needed by the LDAS.
#
# $output                 DMGR       IN  DMGR object reference describing all
#                                        output data produced by the LDAS.
#
# $config               CONFIG       IN  CONFIG object reference describing all
#                                        runtime configuration parameters.
#
# $rc                  integer      OUT  function return value:
#
#                                        0: waiting for LDAS input or output
#                                           data (i.e. batch job in queue).
#                                              
#                                        1: LDAS complete for the specified
#                                           time.
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler referent.
#
# $exception           integer      OUT  Thrown Exceptions:
#
#                                        0: No exception occurred.
#
#                                        3: No more retries. LDAS failed to
#                                           complete successfully
#
#                                        4: Required data is missing or below
#                                           expected volume
#
#                                        5: Ancillary data is missing or below
#                                           expected volume
#
#                                        6: Unable to create directory
#
#                                        7: Model run failed
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           11/01/2013      J.Ardizzone  created.
#           10/07/2014      J.Ardizzone  documented.
#******************************************************************************

  FUNCTION_NAME: my $this = "runModel";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $inQueue;
  my %options = ();
  my $nextday = $time + 86400;

  my @bcs = qw/BCS_CLSM BCS_RTM/;
  my @scaling = qw/SCALE_SPL1CTB SCALE_SPL2SMAP/;
  my @lfo = qw/LFO_INST LFO_TAVG LFO_CORR/;
  my @required = (@bcs, @scaling, @lfo, "CLIM");
  my @smap = qw/SPL1CTB SPL2SMA SPL2SMAP/;
  my @inputs = (@required, @smap);
# my @outputs = qw/AUP GPH LMC RC_OUT RESTART TILE/;
  my @outputs = qw/GPH LMC RC_OUT TILE/;

  my $run_path = $config->{L4_SM_RUN};
  my $rip_path = $config->{L4_SM_RIP};
  my $job      = File::Spec->catfile($run_path,"*.j");

  my $sys = SYSTEM->new($config->{L4_SM_MODEL},%$config);

# Exit if the model has already completed
# or if it is disabled.
# =======================================

  $sys->isEnabled() or return $eh->advisory(1);

  BOOLEAN: my $failed;
  BOOLEAN: my ($success, $expired) = ($output->mstat(\@outputs))[0,1];

  return 2 if $success;

# Advise and return if the model is still
# running.
# ====================================

  ($inQueue) = $sys->queryBatch();
  if ($expired && $inQueue) { $eh->warning(2); return 0 }
  $eh->comment("Waiting for LDAS to complete") if $inQueue;
  $inQueue and return 0;

# Finalize and exit if the
# model has completed.
# ========================

  $output->mexecute(\@outputs) if $sys->isOpenProcess();

  $success = ($output->mstat(\@outputs))[0];

  $failed  = ($sys->isOpenProcess() and ! $success);

  $success   and do {

                   $output->execute("AUP");
                   $output->execute("RESTART");
                   $sys->setStatus(SUCCESS=>1);
                   $eh->comment("Model successfully completed");
                   return 1
                 };

  $failed    and do {

                   $sys->setStatus(FAILED=>1);
                   $eh->warning(7);
                 };

# Retrieve all of the inputs
# ==========================

  $input->mexecute(\@inputs,\%options) or return 0;
  is_SMAP_Complete($time,$input,$config,@smap) or return 0;
  is_SMAP_Complete($time+86400,$input,{%$config,OP_IDLE=>0},@smap) or return 0;

# Execute cross-stream data acquisition (if needed)
# =====================================

  my $dictionary = $config->{DICTIONARY};

  $dictionary->{CROSS_STREAM} and do {

    $dictionary->{L4_SM_LFO_version} = $dictionary->{CROSS_STREAM};
    $input->mexecute(\@lfo,\%options) or return 0;
    $dictionary->{L4_SM_LFO_version} = $config->{L4_SM_LFO_version};

  };

# Check for required inputs
# =========================

  foreach my $type (@required) 
  { ($input->stat($type))[0] or $eh->error(4,TYPE=>$type) }

  return $eh->throw() if $eh->isError();

# Open a new process. Exit
# if retries have been exhausted.
# ===============================

  $sys->openProcess() or return $eh->error(3);

# Ancillary inputs

  foreach my $type (@smap) {
    ($input->stat($type))[0] or $eh->warning(5,TYPE=>$type);
    listSMAPFiles($input->{$type});
  }

# Create the model output directories
# and clear old files.
# ===================================

  foreach my $type (keys %$output) {

    my $dtr      = $output->{$type};
    my @template = $dtr->files({SOURCE=>1});

    foreach my $template (@template) {
      
      my ($file, $dir);

      $file = $time->strftime($template);
      $dir  = dirname $file;
      makepath($dir) or return $eh->fatal(6,PATH=>$dir);

#     unlink $file or warn "failed to remove file: $!\n" if $type eq "RC_OUT";

      $file = $nextday->strftime($template);
      $dir  = dirname $file;
      makepath($dir) or return $eh->fatal(6,PATH=>$dir);

    }

  }

# Configure and execute the
# model.
# =========================

  rmtree($run_path) if (-e $run_path and -d $run_path);
  makepath($run_path) or return $eh->fatal(6,PATH=>$run_path);

  my $nml_files = File::Spec->catfile($rip_path, "*.nml");
  my $history   = File::Spec->catfile($rip_path, "HISTORY.rc");
  
  $config->jobConfig($rip_path,$run_path,%options); 
  $config->jobConfig($nml_files,$run_path,%options,TIMELESS=>1);
  $config->jobConfig($history,$run_path,%options,TIMELESS=>1);

  $eh->comment("Submitting the model batch job");

  $sys->submitBatch($job);

  return 0;

}

sub is_SMAP_Complete { my $time   = shift;
                       my $input  = shift;
                       my $config = shift;
                       my @smap   = scalar(@_) ? @_ : ();

  ERROR_HANDLE:  my $this = "is_SMAP_Complete";
  ERROR_HANDLER: my $eh   = ERROR::Handler->new(\&error_handler, $this);

  $config->{OP_FORCE} and return 1;
  $config->{OP_IDLE}  or  return 1;

  my $now  = CLOCK->new();
  my $date = $time->strftime("%Y%m%d");
  my $wait = ($input->mstat(\@smap, DATE=>$date))[3];

  foreach my $type (@smap) {

    my $dtr       = $input->{$type};
    my @pdr       = $dtr->search(DATE=>$date);

    my @ctime     = map { (stat($_))[10] } @pdr;
    my $last_time = (sort @ctime)[-1];

    my $elapsed_time = $now->epoch - $last_time;

    $eh->comment("Time since last $type file: $elapsed_time seconds");

    $wait = 1 if $elapsed_time <= $config->{OP_IDLE};

  }

  $wait or return 1;

  $eh->comment("Waiting for possible late SMAP files.");
  return 0;

}

sub listSMAPFiles { DTR: my $dtr = shift;

  $dtr or return;
  my ($tstart, $tend) = $dtr->times();

  CLOCK: my $t = $tstart;

  my $day     = $tstart->strftime("%Y%m%d");
  my $end_day = $tend->strftime("%Y%m%d");

  while ($day <= $end_day) {

    my $date = $t->strftime("%Y%m%d");

    my $dtr_local = DTR->new($dtr->{DTR_FILE_ID},%$dtr,DATE=>$date,TIME=>0);

    makeSMAPList($dtr_local);

    $t += 86400;
    $day = $t->strftime("%Y%m%d");

  }

  return;

}

sub makeSMAPList { DTR: my $dtr = shift;

  $dtr or return;

  my ($fh_A, $fh_D);

  my $date = $dtr->{DATE};
  my @pdr  = $dtr->search(DATE=>$date);

  my @file_list;
  foreach my $pdr (@pdr) { push @file_list, ($dtr->files($pdr)) };

  my $dictionary = $dtr->{DICTIONARY};
  my $name       = $dtr->{SMAP_LONG_NAME};
  my $path       = dirname $dtr->{FILE_LIST_OUT};
  my $pathname   = File::Spec->catfile($path, $name);
  my $sl         = SMAPLIST->new(%$dtr, %$dictionary);

  makepath($path);

# Eliminate duplicate and blacklisted orbit files.
# ================================================

  my @files = $sl->blacklist("Radiometer", @file_list);

# Create file listings of SMAP granules
# for the LDAS.
# =====================================

  open $fh_A, ">$pathname" . "_A_list.txt";
  open $fh_D, ">$pathname" . "_D_list.txt";

  foreach my $file (@files) {

    $name = basename $file;

    $name =~ /(SMAP_.*)_\d{5}_D_/ and do { print $fh_D "$name\n"; next };
    $name =~ /(SMAP_.*)_\d{5}_A_/ and do { print $fh_A "$name\n"; next };

  }

  close $fh_A;
  close $fh_D;

  return;

}

sub export { CONFIG: my $config = shift;

  $config->{OP_EXPORT} or  return 1;
  $config->{OP_NO_HDF} and return 1;
  $config->{OP_NO_POST} and return 1;

  FUNCTION_NAME: my $this = "export";
  ERROR_HANDLER: my $eh   = ERROR::Handler->new(\&error_handler,$this);

  my $export     = DMGR->new($config->{L4_SM_EXPORT},%$config);
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

#******************************************************************************
sub genPCTL  { CONFIG: my $config = shift;
#******************************************************************************
# English Name: Generate Percentile Fields
# -------------
#
# Purpose: Creates percentile data fields from the LDAS GPH binary outputs
# -------- and appends the derived percentile data to each of the respective
#          GPH files.
#
# Language: Perl
# ---------
#
# See Also: CLOCK.pm, DMGR.pm, CONFIG.pm, ERROR::Handler.pm, &error_handler
# ---------
#
# Usage: $rc = genPCTL($config)
# ------ $exception = $eh->isError();
#
# Caveats: 1. Running this code more than once without recreating the GPH
# --------    output from the LDAS could result in unwanted side effects such
#             as multiple concatenations of the percentile fields to the GPH
#             binary files.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $config               CONFIG       IN  CONFIG class hash referent describing
#                                        the system configuration and state. 
#
#  {L4_SM_OUTPUT}                    IN  root directory for the LDAS output
#                                        data transaction.
#
#  {L4_SM_IMPORT}                     *  root directory for the data stream
#                                        import transactions.
#
#  {DATE}                             *  current processing date as ccyymmdd.
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
#                                        1: Missing data transaction file
#                                        2: File or directory does not exist
#                                        3: Error generating percentiles
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/16/2014      J.Ardizzone  created.
#******************************************************************************

  FUNCTION_NAME: my $this = "genPCTL";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

# $config->{OP_NO_POST} and return 1;

# Retrieve the input and output filenames.
# ========================================

  my $output   = DMGR->new($config->{L4_SM_OUTPUT},%$config);
  my $import   = DMGR->new($config->{L4_SM_IMPORT},%$config);
  my $time     = CLOCK->new(DATE=>$config->{DATE}, TIME=>0);

  my $datetime = $time->datetime;
  $eh->comment( "Generating percentile data: " .
                "Collection = GPH, " .
                "Time = $datetime");

  $output->{"GPH"}  or return $eh->error(1,TYPE=>"GPH");
  $output->{"TILE"} or return $eh->error(1,TYPE=>"TILE");
  $import->{"CLIM"} or return $eh->error(1,TYPE=>"CLIM");

# Retrieve the input and output
# filenames and directories.
# =================================

  my @gph_files = $output->files("GPH");
  my $gph_dir   = dirname $gph_files[0];
  my $clim_file = ($import->files("CLIM"))[0];
  my $tile_file = ($output->files("TILE",FILE_TYPE=>"TILE_FILE"))[0];

# Generate the percentile data.
# =============================

  -e $gph_dir or return $eh->error(2,FILE=>$gph_dir);
  -s $clim_file or return $eh->error(2,FILE=>$clim_file);
  -s $tile_file or return $eh->error(2,FILE=>$tile_file);

  print "$tile_file\n";

  system "prcntl.py $tile_file $clim_file @gph_files"
                         and return $eh->error(3,TYPE=>"GPH");

  return 1;
}

#******************************************************************************
sub genISO
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
{
  my $config = shift;

  return 1 if $config->{OP_NO_HDF};
  return 1 if $config->{OP_NO_POST};

  my $this   = "genISO";
  my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $pc         = $config->{PRODUCT_COUNTER};
  my $dictionary = $config->{DICTIONARY};

  system( "L4_SM_genISO.pl -pc $pc " .
          "$config->{L4_SM_ROOT} " .
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
# Purpose: Generates files used for monitoring data streams for quality
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
#           are three monitoring types:
#
#           a. BROWSE - browse imagery
#           b. QuADS - Quality Assurance of Data Sets (images)
#           c. ODS - observation data stream files used to generate observation
#                    metrics.
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
#  {L4_SM_BROWSE_directoryID}         *  base directory path for storing
#                                        browse imagery (see note-1).
#
#  {L4_SM_QUADS_directoryID}          *  base directory path for storing
#                                        QuADS imagery (see note-1).
#
#  {L4_SM_ODS_directoryID}            *  base directory path for storing
#                                        ODS files (see note-1).
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

  $eh->comment("Generating data for monitoring: Date = $config->{DATE}");

  my $date        = $config->{DATE};
  my $pc          = $config->{PRODUCT_COUNTER};
  my $stream_type = $config->{OP_STREAM_TYPE};
  my $stream_name = $config->{OP_STREAM_NAME};

  my $hasHDF = $config->{OP_NO_HDF} ? 0 : 1;
  my ($outdir, $output);

  $config->{OP_NO_POST} and return 1;

# Generate ODS Files
# ==================

  $outdir = $config->{L4_SM_ODS_directoryID};
  $output = File::Spec->catdir($outdir, "Y%Y", "M%m", "D%d");

  system("L4.pl -ods -st $stream_type -sn $stream_name -date $date " .
     "-pc $pc -o $output") and return $eh->error(1,TYPE=>"ODS") if $outdir;

# Generate OBS2HTML plots
# =======================

  $outdir = $config->{L4_SM_OBS2HTML_directoryID};

  system("L4.pl -o2h -st $stream_type -sn $stream_name -date $date " .
     "-pc $pc -auto") and return $eh->error(1,TYPE=>"OBS2HTML") if $outdir;

# Generate Browse Imagery
# =======================

  $hasHDF and do {

  $outdir = $config->{L4_SM_BROWSE_directoryID};
  $output = File::Spec->catdir($outdir, "Y%Y", "M%m", "D%d");

  system("L4.pl -browse -st $stream_type -sn $stream_name -date $date " .
     "-pc $pc -auto") and return $eh->error(1,TYPE=>"browse") if $outdir;

  };

# Generate QuADS Imagery
# ======================

  $hasHDF and do {

  $outdir = $config->{L4_SM_QUADS_directoryID};
  $output = File::Spec->catdir($outdir, "Y%Y", "M%m", "D%d");

  system("L4.pl -quads -st $stream_type -sn $stream_name -date $date " .
     "-pc $pc -auto") and return $eh->error(1,TYPE=>"QuADS") if $outdir;

  };

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

  $option{L4_STREAM}     = basename $path;
  $option{JOBNAME}       = $option{L4_STREAM};

  $option{L4_SM_ROOT}    = $path;
  $option{L4_SM_PATH}    = File::Spec->catdir($option{L4_SM_ROOT}, "bin");
  $option{L4_SM_IMPORT}  = File::Spec->catdir($option{L4_SM_ROOT}, "import");
  $option{L4_SM_MODEL}   = File::Spec->catdir($option{L4_SM_ROOT}, "runModel");
  $option{L4_SM_OUTPUT}  = File::Spec->catdir($option{L4_SM_MODEL},"output","ldas");

  $option{L4_SM_GEN_ISO} = File::Spec->catdir($option{L4_SM_ROOT}, "genISO");
  $option{L4_SM_EXPORT}  = File::Spec->catdir($option{L4_SM_ROOT}, "export");
  $option{L4_SM_RIP}     = File::Spec->catdir($option{L4_SM_MODEL},"rip");
  $option{L4_SM_RUN}     = File::Spec->catdir($option{L4_SM_MODEL}, "run");

  my $log_file           = File::Spec->catdir($option{L4_SM_ROOT}, "log");
  $ENV{L4_LOG_FILE}      = $ENV{L4_LOG_FILE} // $log_file;
  $option{L4_LOG_FILE}   = $ENV{L4_LOG_FILE};

  $ENV{PATH}     = join ":", $option{L4_SM_PATH}, $ENV{PATH};

# Retrieve the date/time
# from the system clock.
# ======================

  my $tmgr = TMGR->new($option{L4_SM_MODEL});

  $option{DATE} = $tmgr->{DATE};
  $option{TIME} = $tmgr->{TIME};

# Configure the log handler
# =========================

  my $lh = LOG::Handler->new(SYSTEM=>"L4_SM_SPS",
                             APPLICATION=>"L4_SM",
                             DATE=>$option{DATE},
                             TIME=>$option{TIME});

  setTimes(\%option);

# Return the run-time configuration
# =================================

  my $config = CONFIG->new($option{L4_SM_ROOT},%option);

  return $config;

}

#******************************************************************************
sub checkLog { my $time   = shift;
               my $output = shift;
               my $config = shift;
#******************************************************************************

  FUNCTION_NAME: my $this = "checkLog";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  my $dtr = $output->{RC_OUT};
  $dtr or return $eh->error(1,TYPE=>"RC_OUT");

  my @files    = $dtr->files();
  my ($file)   = grep /ldas_log/, @files;
  my $pathname = $config->time_interp($file, $time);

  open my $fh, "< $pathname";

  while (<$fh>) {

    chomp;

    /^LDAS WARNING/ and do { return $eh->error(2,MSG=>$_,THROW=>0) };

  }

  close $fh;

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
#                                        (e.g. SPL4SM_V05005).
#  
#  {OP_STREAM_TYPE}     string      OUT  Name of the stream type to be
#                                        processed (e.g. SPL4SM).
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
  my ($export, $force, $help, $idle, $man, $noHDF, $noHold, $noPost, $pc);
  my ($resume, $run_freq, $stream_name, $stream_type);

# Retrieve command-line options
# =============================

  GetOptions ("help|h" => \$help,
              "man" => \$man,
              "define=s" => \%define,
              "export" => \$export,
              "force|f" => \$force,
              "idle=i" => \$idle,
              "noHDF" => \$noHDF,
              "noPost" => \$noPost,
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
  $options{OP_IDLE}         = $idle // 7200;
  $options{OP_NO_HDF}       = $noHDF;
  $options{OP_NO_POST}      = $noPost;
  $options{OP_NO_HOLD}      = $noHold;
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

sub setTimes
{

  my $config = shift;

  my $date = $config->{DATE};
  my $time = $config->{TIME};

  my $tstart = CLOCK->new(DATE=>$date,TIME=>0);
  my $tend   = $tstart + ONE_DAY;

  $config->{START_YEAR}  = $tstart->year;
  $config->{START_MONTH} = $tstart->mon;
  $config->{START_DAY}   = $tstart->mday;
  $config->{START_HOUR}  = $tstart->hour;
  $config->{START_MIN}   = $tstart->min;
  $config->{START_SEC}   = $tstart->sec;

  $config->{END_YEAR}  = $tend->year;
  $config->{END_MONTH} = $tend->mon;
  $config->{END_DAY}   = $tend->mday;
  $config->{END_HOUR}  = $tend->hour;
  $config->{END_MIN}   = $tend->min;
  $config->{END_SEC}   = $tend->sec;

  $config->{DATE2} = $tend->strftime("%Y%m%d");
  $config->{TIME2} = $tend->strftime("%H%M%S");

  return 1;
}

sub makepath { my $dir = shift; return 1 if -d $dir; mkpath $dir or return 0 }

sub hash2opt { my %hash = @_;

  my $opts = undef;

  foreach my $key (keys %hash) {

    $opts = $opts . "-define " . $key . '="' . $hash{$key} . '" ';

  }

  return $opts;

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
# -------- is a catalog of registered exceptions for all methods contained in
#          this application.
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
#           10/05/2014      J.Ardizzone  new error handling paradigm
#                                        implemented.
#******************************************************************************

  my $type   = $options{ERROR_TYPE};
  my $lh     = LOG::Handler->new(HANDLE=>$error_handle);

# Trap error handler events
# =========================

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

# runModel() error handle
# =======================

  $error_handle eq "runModel" and do {

    $error_code == 1 and do {
      $lh->$type(1,"LDAS is disabled");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Expected execution time for the LDAS has expired.");
      return;
    };

    $error_code == 3 and do {
      $lh->$type(3,"No more retries. LDAS failed to complete successfully");
      return;
    };

    $error_code == 4 and do {
      $lh->$type(4,"Data is missing or below expected volume " .
                                "for type \"$options{TYPE}\"");
      return;
    };

    $error_code == 5 and do {
      $lh->$type(5,"Data is missing or below expected volume " .
                                "for type \"$options{TYPE}\"");
      return;
    };

    $error_code == 6 and do {
      $lh->$type(6,"Unable to create directory: \"$options{PATH}\".");
      return;
    };

    $error_code == 7 and do {
      $lh->$type(7,"Model run failed");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# genPCTL() error handle
# ======================

  $error_handle eq "genPCTL" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Missing transaction for type: \"$options{TYPE}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Input file or directory is missing: \"$options{FILE}\"");
      return;
    };

    $error_code == 3 and do {
      $lh->$type(3,"Percentile generation failed for type: \"$options{TYPE}\"");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# genISO() error handle
# ======================

  $error_handle eq "genISO" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Error generating HDF-5 granules");
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
# =====================

  $error_handle eq "export" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Export disabled for type, \"$options{TYPE}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"Export failed for type, \"$options{TYPE}\"");
      return;
    };

    print STDERR "$error_handle: Unknown error: code = $error_code\n";

    return;

  };

# checkLog() error handle
# =======================

  $error_handle eq "checkLog" and do {

    $error_code == 1 and do {
      $lh->$type(1,"Missing transaction for type: \"$options{TYPE}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type(2,"$options{MSG}");
      return;
    };

  };

  print STDERR "No error handle for \"$error_handle\"\n";

  return;

}
