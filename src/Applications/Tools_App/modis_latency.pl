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

  my $latency;
  my $latency_min;
  my $latency_max;
  my $clock = CLOCK->new();

  foreach my $filename (@ARGV) {

    $filename =~ /A(\d{7}).*(\d{13}).hdf/;

    my $data_time = $1;
    my $post_time = $2;

    my $t_data = $clock->strptime($data_time,"%Y%j");
#   my $t_post = $clock->strptime($post_time,"%Y%j%H%M%S");

    my $ctime = (stat($filename))[10];
    my $t_post = gmtime($ctime);

    my $seconds = $t_post->epoch - $t_data->epoch;
    $latency_min = $seconds if ! defined $latency_min;
    $latency_min = $seconds < $latency_min ? $seconds : $latency_min;
    $latency_max = $seconds > $latency_max ? $seconds : $latency_max;
    $latency += $seconds;

  }

  $latency = $latency / @ARGV;

  my $days = $latency_min / 86400;
  my $hours = ($latency_min % 86400) / 3600;
  my $min = (($latency_min % 86400) % 3600) / 60;
  print "Minimum Latency: $days days, $hours hours, $min minutes\n";

  my $days = $latency_max / 86400;
  my $hours = ($latency_max % 86400) / 3600;
  my $min = (($latency_max % 86400) % 3600) / 60;
  print "Maximum Latency: $days days, $hours hours, $min minutes\n";

  my $days = $latency / 86400;
  my $hours = ($latency % 86400) / 3600;
  my $min = (($latency % 86400) % 3600) / 60;
  print "Mean Latency: $days days, $hours hours, $min minutes\n";

  my $num_granules = @ARGV;
  print "Number of Granules: $num_granules\n";

exit 0;
