#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use feature qw(switch say);
  use Data::UUID;

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

  use CONFIG;
  use FIND;
  use DTR;
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

  my $filename = $ARGV[0];
  my $group = $ARGV[1];
  my $meta = METADATA->new();

  my %hash = $meta->h5dump("-g $group $filename");

  print "$group:\n";

  foreach my $key (keys %hash) {

    print "$key|$hash{$key}\n";
  }

exit 0;
