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

  $config->{DICTIONARY} = \%options;
  my $transaction = DMGR->new($path,%$config,%options);
  foreach my $type (keys %$transaction) { $transaction->execute($type) }

exit 0;
