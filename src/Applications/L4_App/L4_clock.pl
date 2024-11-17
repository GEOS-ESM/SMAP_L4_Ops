#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use Time::Seconds;
  use CLOCK;
  use Time::Piece;

  my $fh;
  my $path   = $ARGV[0];
  my $sdate  = $ARGV[1];
  my $stime  = $ARGV[2];
  my $edate  = $ARGV[3];
  my $etime  = $ARGV[4];
  my $incsec = $ARGV[5];

  my $clock = File::Spec->catfile($path,"clock");
  my $time = CLOCK->new(DATE=>$sdate, TIME=>$stime);

  open $fh, ">$clock";
  print $fh "$sdate $stime $sdate $stime $edate $etime $incsec\n";
  close $fh;

  utime $time->epoch, $time->epoch, $clock;
  chmod 0744, $clock;
