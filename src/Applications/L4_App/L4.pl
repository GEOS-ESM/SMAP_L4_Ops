#!/usr/bin/perl

#******************************************************************************
# English Name: Level-4 Dashboard Command
# -------------
#
# The text following this prolog contains documentation for this application
# using Perl POD markup language. Formatted documentation can be obtained
# using the following commands:
#
#        perldoc L4.pl
#        L4.pl -h|-help
#        L4.pl -man
#
#******************************************************************************

=head1 NAME

L4.pl - Provides the functionality for managing the SMAP Level-4 operational system. This is the principle mechanism for both passive (query) and active operations on data streams.

=head1 SYNOPSIS

L4.pl [-h|help] [-man] [after] [-archive] [-before] [-clean] [-daily] [-date date] [-dday] [-disable] [-edate date] [-enable] [-export] [-f|force] [-install] [-list] [-make] [-o output] [-pc counter] [-print type] [-purge] [-recall] [-release] [-remove type] [-replay] [-report type] [-respond] [-rewind] [-run ???] [-sdate date] [-set] [-sn name] [-st type] [-start] [-stop] [-target target] [-task task] [-weekly] [-biweekly]

=head1 OPTIONS

=over 17

=item -h|help

Print a brief help message and exit.

=item -man

Print this command's manual page and exit.

=item -sn|stream_name name

Set the stream name, which is also the name of the last directory in the
fully qualified path location of the stream.

=item -st|stream_type type

Set the stream type, which is also the last parent directory in the fully
qualified path location of the stream.

=item -date date

Set the date as ccyymmdd. See also -dday.

=item -pc counter

Set the product counter. I<counter> is an integer between 1 and 999. The default
value is 1.

=back

=cut

  use strict;

  use Cwd;
  use File::Path qw(mkpath rmtree);
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use File::Which;
  use Getopt::Long;
  use feature qw(switch say);
  use List::Util qw(min max first);

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

  use CONFIG;
  use FIND;
  use DTR;
  use DTR::REPORT;
  use DMGR;
  use FIND;
  use LOG::Handler;
  use METADATA;
  use L4_SM::LOG;
  use L4_SM::OBSLOG;
  use L4_SM::AUP::QA;
  use L4_SM::GPH::QA;
  use L4_SM::LMC::QA;
  use ERROR::Handler;

# Local Variables
# ---------------

  STRING:  my $this = "main";

  CONFIG:  my $config;
  DMGR:    my $input;
  DMGR:    my $output;
  STRING:  my $collection;

# ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler);

  my %passive = ("-list"    => \&listStream,
                 "-print"   => \&printInfo,
                 "-make"    => \&make,
                 "-install" => \&install,
                 "-report"  => \&report,
                 "-browse"  => \&browse,
                 "-quads"   => \&quads,
                 "-ods"     => \&ods,
                 "-o2h"     => \&obs2html);

  my %active  = ("-respond" => \&respond,
                 "-export"  => \&export,
                 "-recall"  => \&purge,
                 "-release" => \&release,
                 "-remove"  => \&remove,
                 "-rewind"  => \&rewind,
                 "-purge"   => \&purge,
                 "-run"     => \&run,
                 "-archive" => \&archive,
                 "-pack"    => \&dtrpack,
                 "-start"   => \&startStream,
                 "-stop"    => \&stopStream,
                 "-enable"  => \&enableTask,
                 "-disable" => \&disableTask,
                 "-set"     => \&setClock);

  my %methods = (%active, %passive);

  my @commands = grep /^-/, @ARGV;
  my @active   = grep { $active{$_} } @commands;
  my %options  = getArgs();

  installGroup(%options) and exit 0;
  makeGroup(%options) and exit 0;
  targetGroup(%options) and exit 0;

  while (nextStream(\%options)) {

    logMessage(%options) if @active;

    foreach my $command (@commands) {

      my $method = $methods{$command};
      $method or next;

      &$method(%options) or exit 1;

    }

  }

exit 0;

sub getArgs
{

  my %options = ();
  my %run_options = ();

  $options{ROOT}     = "/discover/nobackup/projects/gmao/smap/Operations";
  $options{ROOT}     = $ENV{SMAP_OPS_DIR} // $options{ROOT};
  $options{MAKEFILE} = File::Spec->catfile($options{ROOT}, "Makefile");

  my ($after, $archive, $auto, $before, $browse, $clean, $daily, $date, $dday);
  my ($disable, $edate, $enable, $force, $export, $install, $list, $ls, $make);
  my ($o2h, $ods, $output, $pack, $pc, $print, $purge, $quads, $recall,$remove);
  my ($replay, $release, $report, $respond, $rewind, $rm, $rp, $rt, $run, $rw);
  my ($sdate, $set, $short, $sn, $st, $start, $stop, $stream_name);
  my ($stream_type, $target, $task, $weekly, $biweekly);

  $options{OPTIONS} = join " ", @ARGV;
  $options{CMDLINE} = join " ", ("L4.pl", @ARGV);
  $options{RUN}     = (grep /-run/,  @ARGV) ? "default" : undef;

  my @commands = grep /^-/, @ARGV;

  GetOptions ("after" => \$after,
              "archive" => \$archive,
              "auto" => \$auto,
              "before" => \$before,
              "browse" => \$browse,
              "clean" => \$clean,
              "daily" => \$daily,
              "date=i" => \$date,
              "dday" => \$dday,
              "define=s" => \%run_options,
              "disable" => \$disable,
              "edate=i" => \$edate,
              "enable" => \$enable,
              "export" => \$export,
              "force|f" => \$force,
              "install" => \$install,
              "list" => \$list,
              "ls" => \$list,
              "make" => \$make,
              "o=s" => \$output,
              "ods" => \$ods,
              "o2h|obs2html" => \$o2h,
              "pack" => \$pack,
              "pc=i" => \$pc,
              "print=s" => \$print,
              "purge" => \$purge,
              "quads" => \$quads,
              "recall" => \$recall,
              "release" => \$release,
              "remove=s" => \$remove,
              "replay" => \$replay,
              "report=s" => \$report,
              "respond" => \$respond,
              "rewind" => \$rewind,
              "rm" => \$remove,
              "rp" => \$replay,
              "run=s{0,1}" => \$run,
              "rw" => \$rewind,
              "sdate=i" => \$sdate,
              "set" => \$set,
              "short" => \$short,
              "sn=s" => \$stream_name,
              "st=s" => \$stream_type,
              "start" => \$start,
              "stop" => \$stop,
              "stream_name=s" => \$stream_name,
              "stream_type=s" => \$stream_type,
              "target=s" => \$target,
              "task=s" => \$task,
              "weekly" => \$weekly,
              "biweekly" => \$biweekly) or exit 1;

  $options{GROUP}       = $ARGV[0];
  $options{AFTER}       = $after;
  $options{ARCHIVE}     = $archive;
  $options{AUTO}        = $auto;
  $options{BEFORE}      = $before;
  $options{BROWSE}      = $browse;
  $options{CLEAN}       = $clean;
  $options{DAILY}       = $daily;
  $options{DATE}        = $date;
  $options{DEFINE}      = %run_options ? \%run_options : undef;
  $options{DISABLE}     = $disable;
  $options{DDAY}        = $dday;
  $options{EDATE}       = $edate;
  $options{ENABLE}      = $enable;
  $options{EXPORT}      = $export;
  $options{FORCE}       = $force;
  $options{INSTALL}     = $install;
  $options{LIST}        = $list;
  $options{MAKE}        = $make;
  $options{OBS2HTML}    = $o2h;
  $options{ODS}         = $ods;
  $options{OUTPUT}      = $output;
  $options{PACK}        = $pack;
  $options{PRINT}       = isValidString("-print",$print);
  $options{PRODUCT_COUNTER} = $pc;
  $options{PURGE}       = $purge;
  $options{QUADS}       = $quads;
  $options{RECALL}      = $recall;
  $options{RELEASE}     = $release;
  $options{REMOVE}      = isValidString("-remove",$remove);
  $options{REPLAY}      = $replay;
  $options{REPORT}      = isValidString("-report",$report);
  $options{RESPOND}     = $respond;
  $options{RUN}         = $run || $options{RUN};
  $options{REWIND}      = $rewind;
  $options{SDATE}       = $sdate;
  $options{SET}         = $set;
  $options{SHORT}       = $short;
  $options{START}       = $start;
  $options{STOP}        = $stop;
  $options{STREAM_NAME} = isValidString("-sn",$stream_name);
  $options{STREAM_TYPE} = isValidString("-st",$stream_type);
  $options{TARGET}      = isValidString("-target",$target);
  $options{TASK}        = isValidString("-task",$task);
  $options{WEEKLY}      = $weekly;
  $options{BIWEEKLY}    = $biweekly;

  my %cmd_line          = %options;
  $options{CMD}         = \%cmd_line;

  setDefaults(\%options) or return 0;

  return %options;

}

sub isValidString { my $name   = shift;
                    my $string = shift;

  $string or return undef;
  return $string if substr($string,0,1) ne "-";
  print "Missing argument for command-line option: $name\n";
  exit 1;

}

#******************************************************************************
sub setDefaults { my $options = shift;
#******************************************************************************
# English Name: Set Defaults
# -------------
#
# Purpose: Sets default parameters for command-line and runtime parameters.
# -------- 
#
# Language: Perl
# ---------
#
# See Also: ERROR::Handler.pm, LOG::Handler.pm
# --------- setDefaultStreams(), setDefaultTimes()
#
# Usage: $rc = setDefaults($options);
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $options            hash_ref    INOUT  command-line and runtime option
#                                        settings. Unspecified options are
#                                        defined when applicable. 
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
# $exception           integer      OUT  Thrown Exception: pending
#
#
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------
#           08/06/2014      J.Ardizzone  created.
#******************************************************************************

  $options->{PRODUCT_COUNTER} = $options->{PRODUCT_COUNTER} // 1;
  $options->{PRODUCT_COUNTER} = substr($options->{PRODUCT_COUNTER}+1000, 1);

  setDefaultTimes($options) or return 0;
  setDefaultStreams($options) or return 0;

  return 1;
}

#******************************************************************************
sub setDefaultStreams { my $options = shift;
#******************************************************************************
# English Name: Set Default Streams
# -------------
#
# Purpose: Locates the directories associated with the specified stream type
# -------- and stream name. 
#
# Language: Perl
# ---------
#
# See Also: setDefaults(), ERROR::Handler.pm, LOG::Handler.pm
# ---------
#
# Usage: $rc = setDefaultStreams($options);
# ------ $exception = $eh->isError();
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $options            hash_ref    INOUT  command-line and runtime option
#                                        settings:
#
#  {ROOT}               string       IN  Absolute path of the SMAP operational
#                                        root directory.
#
#  {STREAM_TYPE}        string       IN  Stream type (parent directory to the
#                                        stream). An undefined value is 
#                                        equivalent to a wildcard.
#
#  {STREAM}             string       IN  Stream name (sub-directory of the 
#                                        the stream type). An undefined value
#                                        is equivalent to a wildcard.
#
#  {STREAMS}         array_ref      OUT  Array listing of all streams matching
#                                        the stream type and stream name. The
#                                        returned elements are of the form,
#                                        "type/name" and correspond to the
#                                        relative directory path of the stream:
#
#                                        e.g. "SPL4SM/SPL4SM_V05006"
#                                             "SMAP/ORT"
# $rc                  integer      OUT  function return value:
#
#                                        0: invalid or incorrect usage
#                                        1: success
#
#                                        undef: exception occurred.
#
# $eh           ERROR::Handler      OUT  Error handler referent.
#
# $exception           integer      OUT  Thrown Exception: pending
#
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           08/06/2014      J.Ardizzone  created.
#******************************************************************************

  my @streams = ();
  my $ops_root = $options->{ROOT};

# Get a listing of the directories under
# the SMAP Operational Root.
# ======================================

  my $pathname = File::Spec->catfile($ops_root, "*");
  my @stream_type_dirs = < $pathname >;

# Form a list of stream directories matching
# the specified stream type and name.
# ==========================================

  foreach my $stream_type_dir (@stream_type_dirs) {

    -d $stream_type_dir or next;

    my $stream_type_name = basename $stream_type_dir;
    $stream_type_name eq "CVS" and next;

    next if $options->{STREAM_TYPE} and
    $options->{STREAM_TYPE} ne $stream_type_name;

    my $pathname = File::Spec->catfile($stream_type_dir, "*");
    my @stream_name_dirs = < $pathname >;

    foreach my $stream_name_dir (@stream_name_dirs) {

      -d $stream_name_dir or next;

      -f File::Spec->catfile($stream_name_dir, "config") or next;

      my $stream_name = basename $stream_name_dir;
      $stream_name eq "CVS" and next;

      next if $options->{STREAM_NAME} and 
      $options->{STREAM_NAME} ne $stream_name;

      push @streams, File::Spec->catfile($stream_type_name, $stream_name);

    }

  }

  $options->{STREAMS} = \@streams;

  return 1;

}

sub setDefaultTimes { my $options = shift;

  my $today = gmtime();
  my $date  = $options->{DATE}  || $today->strftime("%Y%m%d");
  my $sdate = $options->{SDATE} || $date;
  my $edate = $options->{EDATE} || $sdate;

  $edate = $sdate if $options->{DAILY};
  my $t  = CLOCK->new(DATE=>$sdate, TIME=>0);

  $options->{WEEKLY} and do {

    my $start_of_week = $t - 6 * 86400;

    $edate = $sdate;
    $sdate = $start_of_week->strftime("%Y%m%d");

  };

  $options->{BIWEEKLY} and do {

    my $start_of_week = $t - 13 * 86400;

    $edate = $sdate;
    $sdate = $start_of_week->strftime("%Y%m%d");

  };

  $options->{DATE}  = undef;
  $options->{SDATE} = $sdate;
  $options->{EDATE} = $edate;

# Set exclusive end-date
# ======================

  $t         = CLOCK->new(DATE=>$edate, TIME=>0);
  my $eedate = $t + 86400;

  $options->{EEDATE} = $eedate->strftime("%Y%m%d");

  return 1;

}

sub nextStream { my $options = shift;

  my $streams = $options->{STREAMS};
  my $stream  = shift @$streams;

  $stream or return undef;

  $options->{STREAM_NAME} = basename $stream;
  $options->{STREAM_TYPE} = dirname $stream;
  $options->{STREAM_PATH} = File::Spec->catdir($options->{ROOT},
                                               $options->{STREAM_TYPE},
                                               $options->{STREAM_NAME});

  return $stream;

}

sub nextTime { my $options = shift;

  $options->{DATE} or do {

    $options->{DATE} = $options->{SDATE};
    return $options->{DATE};

  };

  my $time = CLOCK->new(DATE=>$options->{DATE},TIME=>0);

  $time += 86400;
  $options->{DATE} = $time->strftime("%Y%m%d");

  $options->{DATE} = undef if $options->{DATE} > $options->{EDATE};

  return $options->{DATE};

}

sub logMessage { my %options = scalar(@_) ? @_ : ();

  $options{no_log} and return 1;
  $options{AUTO}   and return 1;

  my $cmd = $options{CMD};

  $options{STREAM_NAME} or return 0;
  $options{STREAM_TYPE} or return 0;

  my $stream_name = $options{STREAM_NAME};
  my $stream_path = $options{STREAM_PATH};
  my $log_file    = File::Spec->catfile($stream_path, "log");

  $ENV{L4_LOG_FILE} = $ENV{L4_LOG_FILE} // $log_file;

  my $lh = LOG::Handler->new(SYSTEM=>"L4_SPS",
                             APPLICATION=>"L4.pl",
                             SILENT=>1);

# print "$options{CMDLINE}\n";
  $lh->advisory(1,"Operator issued the following command: $options{CMDLINE}");

  return 1;

}

#******************************************************************************
sub listStream { my %options = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: List Stream
# -------------
#
# Purpose: Prints a listing of data streams managed under the SMAP operational
# -------- root. Name and status information is printed for each requested
#          stream.
#
# Language: Perl
# ---------
#
# Usage: $rc = listStream(%options)
# ------
#
# Prerequisites: getArgs()
# --------------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# %options                HASH       IN  Command-line options and default
#                                        parameters.
#
#  {LIST}              BOOLEAN       IN  1: a listing has been requested
#                                        0: no listing requested
#
#  {STREAM}             STRING        *  Name of the stream (default: all)
#
#  {STREAM_TYPE}        STRING        *  Name of stream type (default: all)
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           06/16/2014      J.Ardizzone  created.
#******************************************************************************

  my $cmd = $options{CMD};
  $options{LIST} or return 1;

# Identify stream
# ===============

  my $stream_type = $options{STREAM_TYPE};
  my $stream_name = $options{STREAM_NAME};
  my $stream_path = $options{STREAM_PATH};
      
  print "Stream type: $stream_type\n";
  print "Stream: $stream_name\n";

# Print system status
# ===================

  my $sys = SYSTEM->new($stream_path);
  my $enable = $sys->isEnabled() ? "Enabled" : "Disabled";
  print "Status: $enable\n";

# Print clock status
# ==================

  -f File::Spec->catfile($stream_path, "clock") and do {
    my $tmgr = TMGR->new($stream_path);
    my $running = $tmgr->isClockRunning() ? "Running" : "Stopped";
    print "Clock: $running: $tmgr->{DATE}, $tmgr->{TIME}\n";
  };

  print "\n";

  return 1;

}

#sub monitor { my %options = scalar(@_) ? @_ : ();

# my $cmd = $options{CMD};
# $options{MONITOR} or return 1;

# my $this = "browse";
# my $eh   = ERROR::Handler->new(\*STDERR, $this);

# $options{TASK} = $options{TASK} // "genISO";
# my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

# -d $task_path or return 1;

# my $cwd  = getcwd();

# $options{STREAM_PATH} eq "SPL4SM" and do {

#   quads(%options, QUADS=>1) or return;
#   browse(%options, BROWSE=>1) or return;
#   ods(%options, ODS=>1) or return;

# };

# $options{STREAM_PATH} eq "SPL4C" and do {

#   quads(%options, QUADS=>1) or return;
#   browse(%options, BROWSE=>1) or return;

# };

sub browse { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{BROWSE} or return 1;

  my $this = "browse";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $options{TASK} = $options{TASK} // "genISO";
  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  my $cwd  = getcwd();

  while (nextTime(\%options)) {

    my $date        = $options{DATE};
    my $pc          = $options{PRODUCT_COUNTER} // "001";
    my $config      = CONFIG->new($task_path,DATE=>$date,PRODUCT_COUNTER=>$pc);
    my $transaction = DMGR->new($task_path,%$config);

    $options{AUTO} and return 1 if ! $config->{L4_BROWSE_directoryID};

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};
      my ($tstart, $tend, $incsec) = $dtr->times();

      for (my $t = $tstart; $t <= $tend; $t += $incsec) {

        my $outdir     = undef;
        my ($template) = $dtr->files({FILE_TYPE=>"SCIENCE"});
        my $file       = $t->strftime($template);

        -s $file or next;

#       Set output directory (use configured settings
#       if -auto specified).
#       =============================================

        if ($options{AUTO}) {

          $outdir = $dtr->{L4_BROWSE_directoryID};
          $outdir = File::Spec->catdir($outdir,"Y%Y","M%m","D%d") if $outdir;

        } else { $outdir = $options{OUTPUT} // $cwd }

        $outdir = $t->strftime($outdir);

        my $rc_path = dirname which("browse.py");
        $rc_path    = File::Spec->catdir($rc_path, "etc", "cmap");

        -s $file or next;

        makepath($outdir) or return $eh->fatal(2,errstr=>"Unable to create " .
                                                "directory: \"$outdir\"");

        system("Browse.csh -f $file -c $rc_path -o $outdir") and 
        return $eh->error(1,errstr=>"Error returned from browse application");

      } 

    }

    mirror(%options, LOCAL_DIR=>$config->{L4_BROWSE_directoryID},
                     LOCAL_FILES=>"*.png",
                     REMOTE_LOGIN=>$config->{L4_BROWSE_EXPORT_LOGIN},
                     REMOTE_DIR=>$config->{L4_BROWSE_EXPORT_DIR});

  }
                  

  return 1;

}

sub obs2html { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{OBS2HTML} or return 1;

  my $this = "obs2html";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $options{STREAM_NAME} =~ /(.+)_(.+)/ or 
  return $eh->error(1,errstr=>"Cannot extract science version id from: " .
                                                "\"$options{STREAM_NAME}\"");

  my $version_id = $2;
  my $cwd = getcwd();

  while (nextTime(\%options)) {

    my $date   = $options{DATE};
    my $pc     = $options{PRODUCT_COUNTER} // "001";
    my $config = CONFIG->new($options{STREAM_PATH},DATE=>$date,
                                                  PRODUCT_COUNTER=>$pc);

    $options{AUTO} and return 1 if ! $config->{L4_OBS2HTML_directoryID};

#   Set output directory (use configured settings
#   if -auto specified).
#   =============================================

    my $local_root;
    my $export_root = $config->{L4_OBS2HTML_EXPORT_DIR};

    if ($options{AUTO}) { $local_root = $config->{L4_OBS2HTML_directoryID} }
    else { $local_root = $options{OUTPUT} // $cwd }

    makepath($local_root) or return $eh->fatal(2,errstr=>"Unable to create " .
                                                 "directory: \"$local_root\"");

    system("o2h.csh -e $version_id -d $date -o $local_root") and
      return $eh->error(3,errstr=>"Error returned from obs2html application");

    my $local_dir  = File::Spec->catdir($local_root,$version_id,"Y%Y","M%m");
    my $export_dir = File::Spec->catdir($export_root, "Y%Y","M%m");
    my $local_cov_dir  = File::Spec->catdir($local_dir, "cov");
    my $export_cov_dir = File::Spec->catdir($export_dir, "cov");

    mirror(%options, LOCAL_DIR=>$local_dir,
                     LOCAL_FILES=>"*.png",
                     REMOTE_LOGIN=>$config->{L4_OBS2HTML_EXPORT_LOGIN},
                     REMOTE_DIR=>$export_dir,
                     NO_APPEND=>1);

    mirror(%options, LOCAL_DIR=>$local_dir,
                     LOCAL_FILES=>"*.html",
                     REMOTE_LOGIN=>$config->{L4_OBS2HTML_EXPORT_LOGIN},
                     REMOTE_DIR=>$export_dir,
                     NO_APPEND=>1);

    mirror(%options, LOCAL_DIR=>$local_cov_dir,
                     LOCAL_FILES=>"$date*.png",
                     REMOTE_LOGIN=>$config->{L4_OBS2HTML_EXPORT_LOGIN},
                     REMOTE_DIR=>$export_cov_dir,
                     NO_APPEND=>1);

  }

  return 1;

}

sub quads { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{QUADS} or return 1;

  my $this = "quads";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $options{TASK} = $options{TASK} // "genISO";
  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  my $cwd  = getcwd();

  while (nextTime(\%options)) {

    my $date        = $options{DATE};
    my $pc          = $options{PRODUCT_COUNTER} // "001";
    my $config      = CONFIG->new($task_path,DATE=>$date,PRODUCT_COUNTER=>$pc);
    my $transaction = DMGR->new($task_path,%$config);

    $options{AUTO} and return 1 if ! $config->{L4_QUADS_directoryID};

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};
      my ($tstart, $tend, $incsec) = $dtr->times();

      for (my $t = $tstart; $t <= $tend; $t += $incsec) {

        my $outdir     = undef;
        my ($template) = $dtr->files({FILE_TYPE=>"SCIENCE"});
        my $file       = $t->strftime($template);

        -s $file or next;

#       Set output directory (use configured settings
#       if -auto specified).
#       =============================================

        if ($options{AUTO}) {

          $outdir = $dtr->{L4_QUADS_directoryID};
#         $outdir = File::Spec->catdir($outdir,"Y%Y","M%m","D%d") if $outdir;

        } else { $outdir = $options{OUTPUT} // $cwd }

        $outdir = $t->strftime($outdir);

        my $rc_path = dirname which("quads.py");
        my $json    = File::Spec->catfile($rc_path, "etc", "smap.json");

        print "$file\n";
        makepath($outdir) or return $eh->fatal(2,errstr=>"Unable to create " .
                                                "directory: \"$outdir\"");

        system("QuADS.csh -f $file -j $json -o $outdir") and 
        return $eh->error(1,errstr=>"Error returned from QuADS application");

      } 

    }

    mirror(%options, LOCAL_DIR=>$config->{L4_QUADS_directoryID},
                     LOCAL_FILES=>"*.png",
                     REMOTE_LOGIN=>$config->{L4_QUADS_EXPORT_LOGIN},
                     REMOTE_DIR=>$config->{L4_QUADS_EXPORT_DIR});

    mirror(%options, LOCAL_DIR=>$config->{L4_QUADS_directoryID},
                     LOCAL_FILES=>"*.html",
                     REMOTE_LOGIN=>$config->{L4_QUADS_EXPORT_LOGIN},
                     REMOTE_DIR=>$config->{L4_QUADS_EXPORT_DIR});

  }

  return 1;

}

sub ods { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{ODS} or return 1;

  my $this = "ODS";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $options{TASK} = $options{TASK} // File::Spec->catdir("runModel",
                                                        "output",
                                                        "ldas",
                                                        "aup");

  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  my $cwd  = getcwd();

  while (nextTime(\%options)) {

    my $date        = $options{DATE};
    my $pc          = $options{PRODUCT_COUNTER} // "001";
    my $config      = CONFIG->new($task_path,DATE=>$date,PRODUCT_COUNTER=>$pc);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};
      my ($tstart, $tend, $incsec) = $dtr->times();

      for (my $t = $tstart; $t <= $tend; $t += $incsec) {

        my ($template) = $dtr->files({FILE_TYPE=>"ObsFcstAna"});
        my $file       = $t->strftime($template);

        -s $file or next;

        $template  = $options{OUTPUT} // $cwd;
        my $outdir = $t->strftime($template);

        print "$file\n";

        makepath($outdir) or return $eh->fatal(2,errstr=>"Unable to create " .
                                                "directory: \"$outdir\"");

        system("ods.py -f $file -o $outdir") and 
        return $eh->error(1,errstr=>"Error returned from ods.py");

      } 

    }

  }

  return 1;

}

sub mirror { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};

  $options{AUTO} or return 1;
  $options{DATE} or return 1;

  $options{LOCAL_DIR}    or return 1;
  $options{LOCAL_FILES}  or return 1;
  $options{REMOTE_LOGIN} or return 1;
  $options{REMOTE_DIR}   or return 1;
  $options{DATE}         or return 1;

  my @login;
  my $date = $options{DATE};
  my $t    = CLOCK->new(DATE=>$date,TIME=>0);

  @login       = split /:/, $options{REMOTE_LOGIN};
  my $protocol = $login[0];

  @login       = split /@/, $login[1];
  my $user     = substr($login[0],2);
  my $machine  = $login[1];

  my $local_tmpl  = $options{LOCAL_DIR};
  my $remote_tmpl = $options{REMOTE_DIR};

  $options{NO_APPEND} or do {
    $local_tmpl  = File::Spec->catdir($local_tmpl,"Y%Y","M%m","D%d");
    $remote_tmpl = File::Spec->catdir($remote_tmpl,"Y%Y","M%m","D%d");
  };

# Send all files in the local directory since
# the exact filenames are not known a priori.
# However, limit the filetypes to .png and .ods.
# ==============================================

  my $local_dir  = $t->strftime($local_tmpl);
  my $remote_dir = $t->strftime($remote_tmpl);
  system("ssh -l $user $machine 'mkdir -p $remote_dir'");

  my $files       = $options{LOCAL_FILES};
  my $local_files = File::Spec->catfile($local_dir, $files);
  system("scp $local_files $user\@$machine:$remote_dir");

  $options{NO_APPEND} and return 1;

# A data-day for soil moisture includes
# 00Z of the next day.
# ======================================

  $options{STREAM_TYPE} eq "SPL4SM" or return 1;

  $t += 86400;

  $local_dir  = $t->strftime($local_tmpl);
  $remote_dir = $t->strftime($remote_tmpl);
  system("ssh -l $user $machine 'mkdir -p $remote_dir'");

  $files       = $options{LOCAL_FILES};
  $local_files = File::Spec->catfile($local_dir, $files);
  system("scp $local_files $user\@$machine:$remote_dir");

  return 1;

}

sub report { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REPORT}      or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my %report;
  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date      = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date);
    my $transactions = DMGR->new($task_path,%$config);

    foreach my $type (sort keys %$transactions) {

      my $dtr = $transactions->{$type};

      $report{$type} = $report{$type} // DTR::REPORT->new();
      $report{ALL_TYPE} = $report{ALL_TYPE} // DTR::REPORT->new();

      $report{$type} += $options{DDAY} ? DTR::REPORT->new($dtr,DATE=>$date) :
                                         DTR::REPORT->new($dtr,PAN_DATE=>$date);

      $report{ALL_TYPE} += $options{DDAY} ? DTR::REPORT->new($dtr,DATE=>$date) :
                                         DTR::REPORT->new($dtr,PAN_DATE=>$date);

    }

  }

  return %report if $options{NO_PRINT};

  printReport(\%report,%options) if $cmd->{REPORT} eq "summary";
  printGranuleReport($report{ALL_TYPE},%options) if $cmd->{REPORT} eq "granule";

  return 1;

}

sub openReport { my $report = shift;
                 my %options = scalar(@_) ? @_ : ();

  my $dtr = $report->{DTR};
  $dtr or return *STDOUT;

  $options{AUTO} or return *STDOUT if ! $options{OUTPUT};

  $options{OUTPUT} =~ /^STDOUT$/i and return *STDOUT;
  $options{OUTPUT} and do { 

    my $fh;
    $options{TARGET} ? open $fh, ">>$options{OUTPUT}" :
                       open $fh, ">$options{OUTPUT}";

    return $fh;
  };

  my $config = CONFIG->new($dtr->{DTR_DIR_ID},%$dtr);
  my $path   = $config->{REPORT_directoryID};
  my $name   = $options{REPORT} eq "summary" ? $config->{REPORT_summaryFileID} :
                                               $config->{REPORT_fileID};

  $path or return *STDOUT;
  $name or return *STDOUT;

  my $now    = gmtime();
  my $time   = $now->strftime("%Y%m%dT%H%M%S");
  my $sdate  = $options{SDATE};
  my $edate  = $options{EDATE};
  my $eedate = $options{EEDATE};

  my $pathname = File::Spec->catfile($path, $name);

  $pathname =~ s/SDATE/$sdate/;
  $pathname =~ s/EEDATE/$eedate/;
  $pathname =~ s/EDATE/$edate/;

  makepath($path) or return *STDOUT;

  open my $fh, ">$pathname";

  return $fh;

}

sub printReport { my $report = shift;
                  my %options = scalar(@_) ? @_ : ();

  my $fh = openReport($report->{ALL_TYPE}, %options);

  my $now    = gmtime();
  my $time   = $now->strftime("%Y%m%dT%H%M%S");

  my $sdate = $options{SDATE};
  my $edate = $options{EDATE};

  foreach my $key (sort keys %$report) {

    my ($days, $hours, $min);

    my $rep          = $report->{$key};
    my @result       = $rep->stat();
    my $granules     = $result[9];
    my $num_granules = @$granules;

    my $data_min_time = $result[10];
    my $data_max_time = $result[11];

    print $fh "Stream Type: $options{STREAM_TYPE}\n";
    print $fh "Stream Name: $options{STREAM_NAME}\n";
    print $fh "Task Name: $options{TASK}\n";
    print $fh "Transaction Name: $key\n";
    print $fh "Period of Performance: $sdate-$edate\n";
    print $fh "Time of Report: $time\n";
    print $fh "Number of PDRs Issued: $result[0]\n";

    ($days, $hours, $min) = @{ $result[1] };
    print $fh "Minimum Production Latency: $days days, $hours hours, $min minutes\n";

    ($days, $hours, $min) = @{ $result[2] };
    print $fh "Maximum Production Latency: $days days, $hours hours, $min minutes\n";

    ($days, $hours, $min) = @{ $result[3] };
    print $fh "Mean Production Latency: $days days, $hours hours, $min minutes\n";

    print $fh "Number of PANs Issued: $result[4]\n";

    ($days, $hours, $min) = @{ $result[5] };
    print $fh "Minimum Acquisition Latency: $days days, $hours hours, $min minutes\n";

    ($days, $hours, $min) = @{ $result[6] };
    print $fh "Maximum Acquisition Latency: $days days, $hours hours, $min minutes\n";

    ($days, $hours, $min) = @{ $result[7] };
    print $fh "Mean Acquisition Latency: $days days, $hours hours, $min minutes\n";

    print $fh "Number of Failed PANs: $result[8]\n";

    $data_min_time = $data_min_time->strftime("%Y-%m-%dT%H:%M:%S")
                                                    if $data_min_time;
    $data_max_time = $data_max_time->strftime("%Y-%m-%dT%H:%M:%S")
                                                    if $data_max_time;

    print $fh "Starting Data-Time Processed: $data_min_time\n";
    print $fh "Ending Data-Time Processed: $data_max_time\n";

    print $fh "Total MBytes: ", $result[12] / 1000000.0, "\n";
    print $fh "Number of Granules: $num_granules\n";

    $options{SHORT} or do { 

      foreach my $granule (@$granules) {

        my $name = basename $granule;
        print $fh "$name\n";

      }

    };

    print $fh "\n";

  }

  close $fh if $fh != *STDOUT;

  return 1;

}

sub printGranuleReport { my $report = shift;
                         my %options = scalar(@_) ? @_ : ();

  my $fh = openReport($report, %options);

  my $dtr = $report->{DTR};
  $dtr or return 0;

  my $config = CONFIG->new($dtr->{DTR_DIR_ID},%$dtr);

  my $now    = gmtime();
  my $time   = $now->strftime("%Y%m%dT%H%M%S");
  my $sdate  = $options{SDATE};
  my $edate  = $options{EDATE};

  my @result       = $report->stat();
  my $granules     = $result[9];
  my @granules     = grep /\.h5$/, @$granules;
  my $num_granules = @granules;

  print $fh "Title: $config->{REPORT_title}\n";
  print $fh "Period of Performance: $sdate-$edate\n";
  print $fh "Time of Report: $time\n";
  print $fh "Number of Granules: $num_granules\n";

  foreach my $granule (sort @granules) {

    my $name = basename $granule;
    print $fh "$name\n";

  }

  close $fh if $fh != *STDOUT;

  return 1;

}

sub respond { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{RESPOND} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  my $config = CONFIG->new($task_path, SDATE=>$options{SDATE},
                                       EDATE=>$options{EDATE},
                                       EEDATE=>$options{EEDATE});

# Retrieve the report contaning
# a listing of retrieved granules
# ===============================

  $config->{REPORT_REMOTE_LOGIN} or return 1;

  getReport($config,%options);

  my $file = File::Spec->catfile($task_path,
                                 "reports",
                                 $config->{REPORT_NAME});

  my @received;

  open my $fh, "<$file";
  while (<$fh>) { chomp; /(.*\.h5)/ or next; push @received, $1 }
  close $fh;

  $config->{RESPONSE_REMOTE_LOGIN} or return 1;

# Create a report on the
# acquired granules.
# ======================

  my %report   = report(%options, REPORT=>1, NO_PRINT=>1);

  my $rep      = $report{ALL_TYPE};
  my $dtr      = $rep->{DTR};
  my @result   = $rep->stat();
  my $granules = $result[9];
  my @archived = grep /\.h5$/, @$granules;
  @archived    = map { basename $_ } @archived;

  $dtr->{REPORT_directoryID} = File::Spec->catdir($task_path, "reports");
  $dtr->{REPORT_fileID}      = $config->{RESPONSE_NAME};

  my $report = $config->clone(RECEIVED=>\@received,
                              ARCHIVED=>\@archived,
                              DTR=>$dtr);

# Create the response file and deliver
# to the inquiring institution. Note: the
# response file is not delivered if an
# output file or handle has been specified.
# =========================================

  printResponse($report, %options);
  $options{OUTPUT} and return 1;

  putReport($config, %options);

  return 1;
}

sub printResponse { my $report = shift;
                    my %options = scalar(@_) ? @_ : ();

  my $fh = openReport($report, %options);

  my $now    = gmtime();
  my $time   = $now->strftime("%Y%m%dT%H%M%S");
  my $sdate  = $options{SDATE};
  my $edate  = $options{EDATE};

  my $archived = $report->{ARCHIVED};
  my $received = $report->{RECEIVED};

  my %archived = ();
  @archived{@$archived} = map { 1 } @$archived;

  my @missing = grep { ! $archived{$_} } @$received;

  my $num_archived = @$archived;
  my $num_missing  = @missing;

  print $fh "Title: $report->{RESPONSE_LONG_NAME}\n";
  print $fh "Period of Performance: $sdate-$edate\n";
  print $fh "Time of Report: $time\n";
  print $fh "Number of Archived Granules: $num_archived\n";
  print $fh "Number of Missing Granules: $num_missing\n";

  foreach my $name (sort @missing) { print $fh "$name\n" }

  close $fh if $fh != *STDOUT;

  return 1;

}

sub getReport { my $config = shift;
                my %options = scalar(@_) ? @_ : ();

  my $dtr = DTR::getProtocol($config->{REPORT_REMOTE_LOGIN}, %$config);
  $dtr or return undef;

  my $file = File::Spec->catfile($config->{REPORT_REMOTE_DIR},
                                 $config->{REPORT_NAME});

  my $dir       = File::Spec->catdir($options{STREAM_PATH},
                                     $options{TASK},
                                     "reports");

  mkpath $dir or return undef if ! -d $dir;

  return $dtr->get($dir,$file);
}

sub putReport { my $config = shift;
                my %options = scalar(@_) ? @_ : ();

  my $dtr = DTR::getProtocol($config->{RESPONSE_REMOTE_LOGIN}, %$config);
  $dtr or return 0;

  my $file = File::Spec->catfile($options{STREAM_PATH},
                                 $options{TASK},
                                 "reports",
                                 $config->{RESPONSE_NAME});

  my $dir = $config->{RESPONSE_REMOTE_DIR};

  -s $file or return 0;

  return $dtr->put($dir,$file);

}

sub stopStream { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{STOP} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $stream_name = $options{STREAM_NAME};
  my $stream_path = $options{STREAM_PATH};

# Stop the clock if the stream
# manages time autonomously.
# ============================

  my $tmgr  = TMGR->new($stream_path);
  $tmgr->stopClock();

# Disable the stream. This will 
# cause concurrent processing to
# halt with the next system query.
# ================================
 
  my $sys = SYSTEM->new($stream_path, JOBNAME=>$stream_name);
  $sys->setStatus(DISABLE=>1);

# Kill any batch jobs that might
# be running under this stream.
# =============================

  $sys->queueDelete();

  return 1;

}

sub startStream { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{START} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

# Enable the stream.
# ==================

  my $stream_path = $options{STREAM_PATH};

  my $sys = SYSTEM->new($stream_path);
  $sys->setStatus(ENABLE=>1);

# Start the clock if the stream
# manages time autonomously.
# =============================

  my $tmgr  = TMGR->new($stream_path);
  $tmgr->startClock();

  return 1;

}

sub enableTask { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{ENABLE} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $task        = $options{TASK};
  my $task_path   = File::Spec->catdir($options{STREAM_PATH}, $task);

  -d $task_path or return 1;

# Enable the task.
# Note: A task could be a stream.
# ===============================

  my $sys = SYSTEM->new($task_path);
  $sys->setStatus(ENABLE=>1);

  return 1;

}

sub disableTask { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{DISABLE} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $task        = $options{TASK};
  my $task_path   = File::Spec->catdir($options{STREAM_PATH}, $task);

  -d $task_path or return 1;

# Enable the task.
# Note: A task could be a stream.
# ===============================

  my $sys = SYSTEM->new($task_path);
  $sys->setStatus(DISABLE=>1);

  return 1;

}

sub setClock { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{SET} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

# Do nothing if the clock is still
# running.
# ================================

  my $tmgr  = TMGR->new($options{STREAM_PATH});

  $tmgr->isClockRunning() and do {

    print "Must stop the stream before resetting the clock\n";
    return 0;

  };

# Set the clock
# =============

  nextTime(\%options) or return 0;
  $tmgr->setClock(%options) or return 0;

  return 1;

}

sub export { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{EXPORT} or return 1;

  $options{STREAM_NAME} or return 0;
  $options{STREAM_TYPE} or return 0;

  my $task      = $options{TASK} // "export";
  my $task_path = File::Spec->catdir($options{STREAM_PATH}, $task);

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $pc = $options{PRODUCT_COUNTER} // "001";
    my $config = CONFIG->new($task_path,DATE=>$date,PRODUCT_COUNTER=>$pc);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) { 

      my $dtr = $transaction->{$type};
      $dtr->{PDR_EXPORT_DIR} or next;

      ($transaction->stat($type))[0] and next;

      my $export_dir = $dtr->{PDR_EXPORT_DIR};
      $dtr->{PDR_EXPORT_DIR} .= "_hold" unless $options{RELEASE};

      $transaction->execute($type);

      $dtr->{PDR_EXPORT_DIR} = $export_dir;

    }

  }

  return 1;

}

sub release { my %options = scalar(@_) ? @_ : ();

  my $this = "release";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  my $cmd = $options{CMD};
  $options{RELEASE} or return 1;

  $options{STREAM_NAME} or return 0;
  $options{STREAM_TYPE} or return 0;

  my $task      = $options{TASK} // "export";
  my $task_path = File::Spec->catdir($options{STREAM_PATH}, $task);

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date,TIME=>0);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};
      $dtr->{PDR_EXPORT_DIR} or next;

#     Locate the PDRs and relocate to
#     the polling directory.
#     ===============================

      my @pdr = $dtr->search();

      my ($path, $name, $in_file, $out_file);

      foreach my $pdr (@pdr) {

        $name     = basename $pdr;

        $path     = $dtr->{PDR_EXPORT_DIR} . "_hold";
        $in_file  = File::Spec->catfile($path,$name);

        $path     = $dtr->{PDR_EXPORT_DIR};
        $out_file = File::Spec->catfile($path,$name);

        -s $in_file or next;

        print "Move $in_file to $out_file\n";

        rename $in_file, $out_file or
        return $eh->error(1,errstr=>"Unable to move $in_file to $out_file");

      }

    }

  }

  return 1;

}

sub remove { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REMOVE} or return 1;

  $options{TASK}   or return 0;

  $options{TARGET} and do { print "-remove not allowed for composite stream\n";
                            return 0;
                          };

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  $options{REMOVE} eq "local" and return removeFiles(%options,LOCAL=>1);
  $options{REMOVE} eq "source" and return removeFiles(%options,SOURCE=>1);
  $options{REMOVE} eq "history" and return removeHistory(%options);

  return 0;
}

sub removeFiles { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REMOVE} or return 1;

  $options{TASK} or return 0;
  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my %fopts = ("LOCAL", $options{LOCAL}, "SOURCE", $options{SOURCE});

  my $task_path = File::Spec->catdir($options{STREAM_PATH}, $options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date,TIME=>0);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};

      $dtr->{CLOCK_METHOD} or next unless $options{FORCE};
      $dtr->{CLOCK_METHOD} eq "timeless" and next unless $options{FORCE};

#     Remove files. Files with root origins
#     outside of the data stream will not be
#     removed.
#     ======================================

      my @files = $transaction->files($type,%fopts);

      foreach my $file (@files) { 

        -f $file and do {
          print "Removing $file\n";
          unlink $file or warn "failed to remove file: $!\n";
        };

      }

    }

  }

  return 1;

}

sub removeHistory { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REMOVE} or return 1;

  $options{TASK} or return 0;
  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $task_path = File::Spec->catdir($options{STREAM_PATH}, $options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date,TIME=>0);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};

      $dtr->{CLOCK_METHOD} or next unless $options{FORCE};
      $dtr->{CLOCK_METHOD} eq "timeless" and next unless $options{FORCE};

#     Remove the PDRs
#     ===============

      my @pdr = $dtr->search();
      my ($path, $name, $pdr, $pan);

      foreach my $file (@pdr) { 

        $path = $dtr->{PDR_LOCAL_DIR};
        $name = basename $file;
        $pdr  = File::Spec->catfile($path, $name);

        $path = $dtr->{PAN_LOCAL_DIR};
        $name =~ s/\.PDR/\.PAN/;
        $pan  = File::Spec->catfile($path, $name);

        -f $pdr and do {
          print "Removing $pdr\n";
          unlink $pdr or warn "failed to remove file: $!\n";
        };

        -f $pan and do {
          print "Removing $pan\n";
          unlink $pan or warn "failed to remove file: $!\n";
        };

        $dtr->{PDR_EXPORT_DIR} or next;

        $path = $dtr->{PDR_EXPORT_DIR};
        $name = basename $file;
        $pdr  = File::Spec->catfile($path, $name);

        $path = $dtr->{PAN_EXPORT_DIR};
        $name =~ s/\.PDR/\.PAN/;
        $pan  = File::Spec->catfile($path, $name);

        -f $pdr and do {
          print "Removing $pdr\n";
          unlink $pdr or warn "failed to remove file: $!\n";
        };

        -f $pan and do {
          print "Removing $pan\n";
          unlink $pan or warn "failed to remove file: $!\n";
        };

      }

    }

  }

  return 1;
}

sub purge { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{PURGE} or $options{RECALL} or return 1;

  $options{TASK} or return 0;
  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $task_path = File::Spec->catdir($options{STREAM_PATH}, $options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date,TIME=>0);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (keys %$transaction) {

      my $dtr = $transaction->{$type};
      $dtr->{PDR_EXPORT_DIR} or next;

#     Purge completed PDRs
#     and associated granules.
#     ========================

      my @pdr = ($options{DDAY} or $options{RECALL}) ?
                $dtr->search() : $dtr->search(PAN_DATE=>$date);

      my ($path, $name, $pathname, $success);

      foreach my $pdr (@pdr) { 

        $path = $dtr->{PAN_EXPORT_DIR};
        $name = basename $pdr;
        $name =~ s/\.PDR/\.PAN/;
        $pathname = File::Spec->catfile($path,$name);

        -f $pathname or next unless $options{RECALL};

        $success = ! system "grep -q SUCCESS $pathname";
        $success or next unless $options{RECALL};

        $path     = $dtr->{PDR_EXPORT_DIR};
        $path    .= "_hold" if $options{RECALL};
        $name     = basename $pdr;
        $pathname = File::Spec->catfile($path,$name);

        -f $pathname or next;

        my @files = $dtr->files($pathname);

        foreach my $file (@files) {

          $file =~ /^$config->{L4_EXPORT_directoryID}/ or next;
          -f $file or next;

          print "Removing: $file\n";
          unlink $file or warn "failed to remove file: $!\n";
        }

        print "Removing: $pathname\n";
        unlink $pathname or warn "failed to remove file: $!\n";

      }

    }

  }

  return 1;
}

sub printInfo { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{PRINT} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $sequence = undef;
  $options{PRINT} =~ /crid/i     and $sequence = "METADATA::";
  $options{PRINT} =~ /comment/i  and $sequence = "\@C";
  $options{PRINT} =~ /advisory/i and $sequence = "\@A";
  $options{PRINT} =~ /command/i  and $sequence = "Operator issued";
  $options{PRINT} =~ /warning/i  and $sequence = "\@W";
  $options{PRINT} =~ /error/i    and $sequence = "\@E";
  $options{PRINT} =~ /fatal/i    and $sequence = "\@F";

  $sequence = $sequence // $options{PRINT};

  my $stream_name = $options{STREAM_NAME};
  my $stream_path = $options{STREAM_PATH};
  my $log_file    = File::Spec->catfile($stream_path, "log");

  my @log = ();
  open my $fh, "< $log_file" or die "cannot read log file: $!";
  while (<$fh>) { chomp; push @log, $_ }
  close $fh;

  while (nextTime(\%options)) {

    my $date    = $options{DATE};
    my $time    = CLOCK->new(DATE=>$date,TIME=>0);
    my $tstring = $time->strftime("%Y-%m-%d");
#   my @array   = sort (grep /^$tstring /, @log);

    my @summary = ();
    my @array   = sort (grep /$sequence/, @log);

    foreach my $entry (@array) {

      my @field  = split /\|/, $entry;
      $field[7] =~ /Operator issued/i and next if $options{PRINT} =~ /advisory/i;
#     print "$options{PRINT} issued at $field[0] for $field[4]: \"$field[7]\"\n";
      $field[4] =~ /$tstring/ or next if $options{DDAY};
      $field[0] =~ /$tstring/ or next unless $options{DDAY};

      if ($options{PRINT} =~ /^crid$/i and $field[3] eq "METADATA::compare") {

        push @summary, $field[7];
        next;

      } 

      next if $options{PRINT} =~ /^crid$/i and $field[3] ne "METADATA::update";

      print "Stream Type: $options{STREAM_TYPE}\n";
      print "Stream Name: $options{STREAM_NAME}\n";
      print "System Name: $field[1]\n";
      print "Application Name: $field[2]\n";
      print "Method Name: $field[3]\n";
      print "Production Time: $field[0]\n";
      print "Data Time: $field[4]\n";
      print "Return Code: $field[6]\n";
      print "Message: $field[7]\n";
      foreach my $index (0..$#summary) {print "($index): $summary[$index]\n" }
      print "\n";

      @summary = ();
    }

  }

  return 1;

}

sub run { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{RUN} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $opts     = $options{FORCE} ? "-f" : undef;
  my $tmgr     = TMGR->new($options{STREAM_PATH});
  my $rc_file  = File::Spec->catfile($options{STREAM_PATH}, ".l4rc");
  my $schedule = File::Spec->catfile($options{STREAM_PATH}, "schedule.rc");

  $rc_file = $schedule if -f $schedule;

  my $app          = $options{RUN};
  my $nrun         = 0;
  my $current_date = $app eq "default" ? $tmgr->{DATE} : $options{SDATE};

  -f $rc_file and do {

    open my $fh, "<$rc_file";

    while (<$fh>) {

      chomp;

      /^\s*#/ and next;
      /(\S+)\s+(\S+)\s+(.*)/ or next;

      my $sdate = $1;
      my $edate = $2;
      my $args  = $3;

      $sdate = $current_date if $sdate eq "*";
      $edate = $current_date if $edate eq "*";

      $sdate =~ /\d{8}/ or next;
      $edate =~ /\d{8}/ or next;

      next if $current_date < $sdate;
      next if $current_date > $edate;

      return 1 if $args =~ /\-pause/;

      $nrun = 1 if $args =~ /\-NR/;
      $args =~ s/\-NR//;

      $opts = join " ", ($opts, $args);

    }

    close $fh;

  };

# Run the specified application with options
# from the resource file.
# ==========================================

  $nrun and do {

    my %run = (default => "L4_SM_NR.pl", genISO => "L4_SM_NR_genISO.pl");
    $run{$app} or return 1;

    system "$run{$app} $options{STREAM_PATH} $opts -date $options{SDATE}" and return 0;

    return 1;

  };

  $options{STREAM_TYPE} eq "SPL4SM" and do {
    
    my %run = (default => "L4_SM.pl", genISO => "L4_SM_genISO.pl");
    $run{$app} or return 1;

    system "$run{$app} $options{STREAM_PATH} $opts -date $options{SDATE}" and return 0;

    return 1;

  };

  $options{STREAM_TYPE} eq "SPL4C" and do {

    my %run = (default => "L4_C.pl", genISO => "L4_C_genISO.pl");
    $run{$app} or return 1;

    system "$run{$app} $options{STREAM_PATH} $opts -date $options{SDATE}" and return 0;
    return 1;

  };

  system "dtr.pl $options{STREAM_PATH}" and return 0;
  return 1;

}

sub define { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{DEFINE} or return 1;

  my $options = $options{DEFINE};
  my $rc_file = File::Spec->catfile($options{STREAM_PATH}, ".l4rc");

  open my $fh, ">$rc_file";

  my $sdate = $options{SDATE};
  my $edate = $options{EDATE};

  print $fh join " ", ($sdate, $edate, values %$options), "\n";

  close $fh;

  return 1;

};

sub rewind { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REWIND} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  my $tmgr  = TMGR->new($options{STREAM_PATH});
  $tmgr->{CLOCK} or return 1;

  $options{EDATE} = $tmgr->{DATE};

  stopStream(%options, STOP=>1, no_log=>1) or return 0;

  rewindSPL4SM(%options) or return 0 if $options{STREAM_TYPE} eq "SPL4SM";
  rewindSPL4C(%options) or return 0 if $options{STREAM_TYPE} eq "SPL4C";

  $options{EDATE} = $tmgr->{EDATE};
  setClock(%options, SET=>1, no_log=>1) or return 0;

  return 1;

}

sub rewindSPL4SM { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{REWIND} or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  foreach my $task (qw/aup gph lmc rc_out tile/) {

    my $task_path = File::Spec->catdir("runModel","output", "ldas", $task);

    remove(%options, no_log=>1, REMOVE=>"source", TASK=>$task_path) or return 0;
    remove(%options, no_log=>1, REMOVE=>"history",TASK=>$task_path) or return 0;

  }

  my $t = CLOCK->new(DATE=>$options{SDATE},TIME=>0);
  $t += 86400;
  my $sdate = $t->strftime("%Y%m%d");

  my $task_path = File::Spec->catdir("runModel", "output", "ldas", "rs");

  remove(%options, no_log=>1, SDATE=>$sdate,
                              REMOVE=>"source",
                              TASK=>$task_path) or return 0;

  remove(%options, no_log=>1, SDATE=>$sdate,
                              REMOVE=>"history",
                              TASK=>$task_path) or return 0;

  $task_path = File::Spec->catdir("genISO","output");

  remove(%options, no_log=>1, REMOVE=>"source", TASK=>$task_path) or return 0;
  remove(%options, no_log=>1, REMOVE=>"history", TASK=>$task_path) or return 0;

  $task_path = File::Spec->catdir($options{STREAM_PATH}, "runModel");
  my $sys = SYSTEM->new($task_path);

  $sys->setStatus(CLEAR=>1);

  return 1;

}

sub rewindSPL4C { my %options = scalar(@_) ? @_ : ();

  my $this = "rewindSPL4C";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  my $cmd = $options{CMD};
  $options{REWIND} or return 1;
  $options{STREAM_TYPE} eq "SPL4C" or return 1;

  $cmd->{STREAM_NAME} or return 0;
  $cmd->{STREAM_TYPE} or return 0;

  remove(%options, no_log=>1, REMOVE=>"source", TASK=>"genISO") or return 0;
  remove(%options, no_log=>1, REMOVE=>"history", TASK=>"genISO") or return 0;
  remove(%options, no_log=>1, REMOVE=>"source", TASK=>"run_MET") or return 0;
  remove(%options, no_log=>1, REMOVE=>"history", TASK=>"run_MET") or return 0;
  remove(%options, no_log=>1, REMOVE=>"source", TASK=>"run_MODIS") or return 0;
  remove(%options, no_log=>1, REMOVE=>"history", TASK=>"run_MODIS") or return 0;

  my $time  = CLOCK->new(DATE=>$options{SDATE},TIME=>0);
  my $sdate = $time->strftime("%Y%m%d");

# Retrieve the filename of the model restart
# file currently present in the data stream.
# ==========================================

  my $import_task = File::Spec->catdir($options{STREAM_PATH}, "import");
  my $transaction = DMGR->new($import_task,DATE=>$sdate,TIME=>0);

  my $dtr = $transaction->{SOC};
  $dtr or return $eh->error(1,errstr=>"The import task is missing " .
                                 "the SOC transaction file for restarts");

  my ($dest_file) = $dtr->files({FILE_TYPE=>"SCIENCE"});

  $dest_file = $time->strftime($dest_file);
  -s $dest_file or $eh->warning(2,errstr=>"This data stream appears " .
                                                    "to be uninitialized");
  my $dest_path   = dirname $dest_file;

# Retrieve the filename of the model
# restart for the start date/time.
# ==================================

  my $model_task = File::Spec->catdir($options{STREAM_PATH}, "run_DAILY");
  $transaction = DMGR->new($model_task,DATE=>$sdate,TIME=>0);

  $dtr = $transaction->{L4_C_DAILY};
  $dtr or return $eh->error(3,errstr=>"The model task is missing " .
                                      "the L4_C_DAILY transaction file");

  my ($src_file) = $dtr->files({FILE_TYPE=>"RESTART"});

  $src_file = $time->strftime($src_file);
  -s $src_file or return $eh->warning(4,errstr=>"There is no available " .
                                                    "restart file for $sdate");

  makepath($dest_path) or return $eh->fatal(5,errstr=>"Unable to create " .
                                                "directory: \"$dest_path\"");
  copy ($src_file,$dest_file) or return $eh->fatal(6,errstr=>"Unable to " .
                                              "create file: \"$dest_file\"");

# Remove model generated files for
# the dates to be reprocessed.
# ================================

  my $task = File::Spec->catdir("run_DAILY", "output");

  remove(%options, no_log=>1, REMOVE=>"source", TASK=>$task) or return 0;
  remove(%options, no_log=>1, REMOVE=>"history", TASK=>$task) or return 0;

  return 1;

}

sub archive { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{ARCHIVE} or return 1;

  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};
    print "Archiving $date\n";

    my $config = CONFIG->new($task_path,DATE=>$date);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (sort keys %$transaction) {

      my $dtr = $transaction->{$type};
      $dtr    = DTR::DMF->new(%$dtr,SCHEDULE=>$dtr->{ARCHIVE_SCHEDULE});

      $dtr->isScheduled() or next;

      my $archive_root = $dtr->{L4_ARCHIVE_directoryID} // 
                                      $ENV{SMAP_ARCHIVE_DIR};
      $archive_root    =~ /\/SMAP_L4$/ or next;

      my @source_files;
      my @local_files;

      if ($options{DDAY}) {

        @source_files = $transaction->files($type,ALL=>1,SOURCE=>1,DATE=>$date);
        @local_files  = $transaction->files($type,ALL=>1,LOCAL=>1,DATE=>$date);

      } else {

        @source_files = $transaction->files($type,ALL=>1,SOURCE=>1,
                                                       PAN_DATE=>$date);
        @local_files  = $transaction->files($type,ALL=>1,LOCAL=>1,
                                                       PAN_DATE=>$date);

      }

      my @files       = grep /\/SMAP_L4\//, (@source_files, @local_files);

      my @archive     = map { &chpath($_,$archive_root) } sort @files;

      foreach my $index (0..$#files) {

        my $dir = dirname $archive[$index];

        $dtr->put($dir, $files[$index]);

      }

    }

  }

  return 1;

}

sub dtrpack { my %options = scalar(@_) ? @_ : ();

  my $cmd = $options{CMD};
  $options{PACK} or return 1;

  my $task_path = File::Spec->catdir($options{STREAM_PATH},$options{TASK});

  -d $task_path or return 1;

  while (nextTime(\%options)) {

    my $date = $options{DATE};

    my $config = CONFIG->new($task_path,DATE=>$date);
    my $transaction = DMGR->new($task_path,%$config);

    foreach my $type (sort keys %$transaction) {

      my $dtr = $transaction->{$type};
      $dtr    = DTR::DMF->new(%$dtr);

      my $archive_root = $dtr->{L4_ARCHIVE_directoryID} //
                                      $ENV{SMAP_ARCHIVE_DIR};
      $archive_root    =~ /\/SMAP_L4$/ or next;

      my @source_files= $transaction->files($type,ALL=>1,SOURCE=>1,DATE=>$date);
      my @local_files = $transaction->files($type,ALL=>1,LOCAL=>1,DATE=>$date);
      my @files       = grep /\/SMAP_L4\//, (@source_files, @local_files);

      my %dirs;
      my @dirs     = map { &chpath(dirname($_), $archive_root) } sort @files;
      @dirs{@dirs} = map { 1 } @dirs;

      foreach my $dir (sort keys %dirs) {

        print "Packing $dir\n";

        $dtr = DTR::DMF->new(%$dtr);
        $dtr->pack($dir);

      }

    }

  }

  return 1;
}

sub chpath { my $target = shift;
             my $root   = shift;

  my $name = basename $target;
  my $path = -d $target ? $target : dirname $target;
  my @path = File::Spec->splitdir($path);
  my $first = first { $path[$_] eq "SMAP_L4" } 0..$#path;

  return File::Spec->catdir($root, @path[$first+1..$#path]) if -d $target;

  return File::Spec->catfile($root, @path[$first+1..$#path], $name);

}

sub installGroup { my %options = scalar(@_) ? @_ : ();

  $options{INSTALL}     or return 0;
  $options{GROUP}       or die "Insufficient information for -install";
  $options{TARGET}      or die "Insufficient information for -install";
  $options{STREAM_TYPE} or die "Insufficient information for -install";
  $options{STREAM_NAME} or die "Insufficient information for -install";

  my $out_link = File::Spec->catfile($options{ROOT},
                                    "SPL4",
                                    $options{GROUP},
                                    "export",
                                    $options{TARGET},
                                    $options{STREAM_TYPE},
                                    $options{STREAM_NAME});

  my $in_link = File::Spec->catfile($options{ROOT},
                                    $options{STREAM_TYPE},
                                    $options{STREAM_NAME},
                                    "export",
                                    $options{TARGET});

  -d $in_link or return 1;

  my $dir = dirname $out_link;
  makepath($dir) or die "Unable to create path: \"$dir\"";

  symlink $in_link, $out_link or die "Unable to create: $out_link -> $in_link";

  return 1;

};

sub makeGroup { my %options = scalar(@_) ? @_ : ();

  $options{MAKE} or return 0;
  $options{GROUP} or die "No group specified for -make operation";

# Initiate the group installation
# ===============================

  $options{TARGET} and do {
    system "make -f $options{MAKEFILE} clients OPTIONS=\"$options{OPTIONS}\"";
    return 1;
  };

# Remove the group tree in preparation
# for a new install.
# =====================================

  my $group_root  = File::Spec->catdir($options{ROOT}, "SPL4", $options{GROUP});
  my $group_config = File::Spec->catfile($group_root, "config");

  rmtree($group_root) if -d $group_root;
  makepath($group_root) or die "Unable to create path: \"$group_root\"";
  open my $fh, ">$group_config"; close $fh;

  system "make -f $options{MAKEFILE} $options{GROUP} OPTIONS=\"$options{OPTIONS}\"";

  return 1;

};

sub targetGroup { my %options = scalar(@_) ? @_ : ();

  $options{GROUP} or return 0;
  $options{TARGET} and return 0;

  system "make -f $options{MAKEFILE} $options{GROUP} OPTIONS=\"$options{OPTIONS}\"";

  return 1;

};

sub makepath { my $dir = shift; return 1 if -d $dir; mkpath $dir or return 0 }
