#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use Pod::Usage;

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

  use CONFIG;
  use FIND;
  use DTR;
  use DMGR;
  use FIND;
  use L4_SM::LOG;
  use L4_SM::OBSLOG;
  use ERROR::Handler;

# Retrieve run-time input
# parameters
# =======================

  my %options = getArgs();

  my $tstart = CLOCK->new(DATE=>$options{SDATE},TIME=>0);
  my $tend   = CLOCK->new(DATE=>$options{EDATE},TIME=>0);

  my $spl1ctb_dir  = File::Spec->catdir($options{SMAP_DATA_DIR}, "SMAP", "OPS",
                                        "L1C_TB", "Y%Y", "M%m", "D%d");

  my $spl2smap_dir = File::Spec->catdir($options{SMAP_DATA_DIR}, "SMAP", "OPS",
                                        "L2_SM_AP", "Y%Y", "M%m", "D%d");

# Reconcile granules over the specified period.
# =============================================

  for (my $time = $tstart; $time <= $tend; $time += 86400) {

    my @times;
    my $file_dir;

    my (@spl1ctb, @spl2smap, @spl2sma);
    my (%spl1ctb, %spl2smap, %spl2sma);

    my (@ldas_spl1ctb, @ldas_spl2smap, @ldas_spl2sma);
    my (%ldas_spl1ctb, %ldas_spl2smap, %ldas_spl2sma);

#   Get a listing of all available SMAP L1-2 granules
#   =================================================

    $file_dir   = $time->strftime($spl1ctb_dir);
    $file_dir   = File::Spec->catfile($file_dir, '*.h5');
    my @spl1ctb = map { basename $_ } < $file_dir >;

    @spl1ctb{@spl1ctb} = map { fileTime($_) } @spl1ctb;

    $file_dir    = $time->strftime($spl2smap_dir);
    $file_dir    = File::Spec->catfile($file_dir, '*.h5');
    my @spl2smap = map { basename $_ } < $file_dir >;

    @spl2smap{@spl2smap} = map { fileTime($_) } @spl2smap;

#   Get a listing of files used by the LDAS
#   =======================================

    my @ldas_spl1ctb  = obsList($options{STREAM_PATH}, $time, "SPL1CTB");
    @times = map { fileTime($_) } @ldas_spl1ctb;
    @ldas_spl1ctb{@times} = @ldas_spl1ctb;

    my @ldas_spl2smap = obsList($options{STREAM_PATH}, $time, "SPL2SMAP");
    @times = map { fileTime($_) } @ldas_spl2smap;
    @ldas_spl2smap{@times} = @ldas_spl2smap;

#   Reconcile granules used versus
#   granules available.
#   ==============================

    SPL1CTB:

    foreach my $file_name (sort keys %spl1ctb) {

      my $file_time = $spl1ctb{$file_name};

      my $used      = ($file_name eq $ldas_spl1ctb{$file_time}) ? "yes" : " no";
      my $stranded  = $ldas_spl1ctb{$file_time} ? " no" : "yes";

      print "$file_name $used $stranded\n";

    }

    SPL2SMAP:

    foreach my $file_name (sort keys %spl2smap) {

      my $file_time = $spl2smap{$file_name};

      my $used     = ($file_name eq $ldas_spl2smap{$file_time}) ? "yes" : " no";
      my $stranded = $ldas_spl2smap{$file_time} ? " no" : "yes";

      print "$file_name $used $stranded\n";

    }

  }

exit 0;

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
# See Also:
# ---------
#
# Prerequisites: $ENV{SMAP_OPS_DIR}
# -------------- $ENV{SMAP_DATA_DIR}
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
#  {STREAM_NAME}     string      OUT  Name of the stream to be processed
#                                        (e.g. SPL4SM_V05005).
#  
#  {STREAM_TYPE}     string      OUT  Name of the stream type to be
#                                        processed (e.g. SPL4SM).
#
#  {STREAM_PATH}      string      OUT  Pathname of the stream directory. This
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
#           04/29/2015      J.Ardizzone  created.
#******************************************************************************
{
  my %options = ();
  my ($help, $man);
  my ($date, $sdate, $edate);
  my ($stream_name, $stream_type);

  my $now = CLOCK->new();

# Retrieve command-line options
# =============================

  GetOptions ("help|h" => \$help,
              "man" => \$man,
              "stream_name|sn=s" => \$stream_name,
              "stream_type|st=s" => \$stream_type,
              "sdate=i" => \$sdate,
              "edate=i" => \$edate,
              "date=i"  => \$date);

  my @path = File::Spec->splitdir( $ARGV[0] );

  $options{HELP}  = $help;

  $options{DATE}  = $date // $now->strftime("%Y%m%d");
  $options{SDATE} = $sdate // $date;
  $options{EDATE} = $edate // $options{SDATE};

  $options{STREAM_NAME} = $stream_name // pop @path;
  $options{STREAM_TYPE} = $stream_type // pop @path;
  $options{STREAM_PATH} = $ARGV[0] // File::Spec->catdir($ENV{SMAP_OPS_DIR},
                                         $options{STREAM_TYPE},
                                         $options{STREAM_NAME});

  $options{SMAP_OPS_DIR}  = $ENV{SMAP_OPS_DIR};
  $options{SMAP_DATA_DIR} = $ENV{SMAP_DATA_DIR};

  $help and pod2usage(1);
  $man  and pod2usage(-verbose => 2);

  -d $options{STREAM_PATH} or do {
    print "The stream directory does not exist. Please check arguments.\n";
    exit 1;
  };

  $options{SMAP_OPS_DIR} and $options{SMAP_DATA_DIR} or do {
    print "Must source the smaprc file before running this application\n";
    exit 1;
  };

  return %options;

}

sub fileTime { my $file = shift;

  my $clock = CLOCK->new();
  my $time  = $clock->smap_time($file,DATETIME=>1);
  return $time->epoch;

}

sub obsList { my $stream_path = shift;
              my $time        = shift;
              my $type        = shift;

  my $path = File::Spec->catdir($stream_path,
                                "runModel",
                                "output",
                                "ldas");

  my @obs = ();
  my $config = CONFIG->new($path);

  TIME: for (my $t = $time - 86400; $t <= $time; $t += 86400) {

    my $date     = $t->strftime("%Y%m%d");
    my $input    = DMGR->new($path,%$config,DATE=>$date);
    DTR: my $dtr = $input->{RC_OUT};

    $dtr or next;

#   Retrieve the OBSLOG file information
#   ====================================

    my @files = $dtr->files();
    my $file  = (grep /ldas_obslog.*\.txt$/, @files)[0];
    my $pathname = $t->strftime($file);

    -f $pathname or next;

    my $obslog = L4_SM::OBSLOG->new($pathname);
    $obslog->read($pathname);

#   Retrieve information on the SMAP
#   observations used by the LDAS for
#   the specified date/time.
#   =================================

    my $hash;
    my ($obs_A, $obs_D);

    $type eq "SPL1CTB" and do {

      $hash = $obslog->{SMAP_L1C_Tbh_D} // {};
      $obs_D = $hash->{FILENAMES} // [];
      $hash = $obslog->{SMAP_L1C_Tbh_A} // {};
      $obs_A = $hash->{FILENAMES} // [];

      push @obs, map { basename $_ } (@$obs_D, @$obs_A);

      next TIME;

    };

    $type eq "SPL2SMAP" and do {

      $hash = $obslog->{SMAP_L2AP_Tbh_D} // {};
      $obs_D = $hash->{FILENAMES} // [];

      push @obs, map { basename $_ } @$obs_D;

      next TIME;

    };

    print "obsList: unknown SMAP data type specified\n";
    return ();

  }

  return @obs;

}
