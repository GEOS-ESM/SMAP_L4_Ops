#!/usr/bin/perl

  use strict;

  use Net::SCP;
  use File::Spec;
  use File::Basename;
  use File::Path;
  use Getopt::Std;
  use Getopt::Long;
  use CLOCK;
  use CONFIG;
  use DMGR;
  use DTR;

# Retrieve command-line options
# =============================

  my %options;
  GetOptions ("define=s" => \%options);

  my $path     = $ARGV[0];
  my $config   = CONFIG->new($path);

  $config->{DATE} = $ARGV[1] if defined $ARGV[1];
  $config->{TIME} = $ARGV[2] if defined $ARGV[2];

  exit 1 if ! -d $path;

  my $transaction = DMGR->new($path,%$config,%options);

  foreach my $type (keys %$transaction) { 

    $transaction->execute($type);
    my $dtr = $transaction->{$type};

    makeSMAPList($dtr);

  }

exit 0;

sub makeSMAPList { DTR: my $dtr = shift;

  $dtr or return;

  my ($fh_A, $fh_D);

  my $date = $dtr->{DATE};
  my @pdr  = $dtr->search(DATE=>$date);

  my @file_list;
  foreach my $pdr (@pdr) { push @file_list, ($dtr->files($pdr)) };

  my $name      = $dtr->{SMAP_LONG_NAME};
  my $path      = dirname $dtr->{FILE_LIST_OUT};
  my $pathname  = File::Spec->catfile($path, $name);

  makepath($path);

  my $clock = CLOCK->new();

# Make sure that the input list to be time
# sorted is first alphabetically sorted to
# yield the latest entry for duplicate granules
# (i.e. same orbit number and time but different
# version).
# ==============================================

  @file_list = sort @file_list;
  @file_list = $clock->sort(\@file_list,%$dtr,UNIQUE=>1,REVERSE=>0);

  open $fh_A, ">$pathname" . "_A_list.txt";
  open $fh_D, ">$pathname" . "_D_list.txt";

  foreach my $file (@file_list) {

    $name = basename $file;

    $name =~ /(SMAP_.*)_\d{5}_D_/ and do { print $fh_D "$name\n"; next };
    $name =~ /(SMAP_.*)_\d{5}_A_/ and do { print $fh_A "$name\n"; next };

  }

  close $fh_A;
  close $fh_D;

  return;

}

sub makepath { my $dir = shift; return 1 if -d $dir; mkpath $dir or return 0 }
