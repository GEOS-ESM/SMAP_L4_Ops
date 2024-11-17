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

  use integer;

  my $count = 0;
  my $latency;
  my $latency_min;
  my $latency_max;
  my $clock = CLOCK->new();

  foreach my $filename (@ARGV) {

    $filename =~ /(\d{8}_\d{4})/;

    my $data_time = $1;
    my $t_data = $clock->strptime($data_time,"%Y%m%d_%H%M");

    my $post_time = (stat($filename))[10];
    my $t_post = gmtime($post_time);

    my $seconds = $t_post->epoch - $t_data->epoch;
#   next if $seconds > 86400 * 2;
#   print "$filename\n" if $seconds > 86400 * 2;

    $count++;

    $latency_min = $seconds if ! defined $latency_min;
    $latency_min = $seconds < $latency_min ? $seconds : $latency_min;
    $latency_max = $seconds > $latency_max ? $seconds : $latency_max;
    $latency += $seconds;

  }

  $latency = $latency / $count;

  my ($days, $hours, $min);

  $days = $latency_min / 86400;
  $hours = ($latency_min % 86400) / 3600;
  $min = (($latency_min % 86400) % 3600) / 60;
  print "Minimum Latency: $days days, $hours hours, $min minutes\n";

  $days = $latency_max / 86400;
  $hours = ($latency_max % 86400) / 3600;
  $min = (($latency_max % 86400) % 3600) / 60;
  print "Maximum Latency: $days days, $hours hours, $min minutes\n";

  $days = $latency / 86400;
  $hours = ($latency % 86400) / 3600;
  $min = (($latency % 86400) % 3600) / 60;
  print "Mean Latency: $days days, $hours hours, $min minutes\n";

  print "Number of Granules: $count\n";

exit 0;
