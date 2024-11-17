#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use feature qw(switch say);

  my @field;
  my @output;
  my $match;
  my ($jobID, $jobName, $text);
  my $stream = "SPL4SM";

  while (<>) {

    chomp;

    exit 0 if /\./;

    @output = `qstat`;
    ($match) = grep /$stream/, @output;

    ($jobID, $jobName) = (split /\s+/, $match)[0,1];

    @output = `qstat -a`;
    ($match) = grep /$jobID/, @output;

    @field = split /\s+/, $match;
    $field[3] = $jobName;

    $text = join "\n", @field;

    print "$text\n";

  }
