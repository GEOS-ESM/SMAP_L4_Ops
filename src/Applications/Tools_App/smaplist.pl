#!/usr/bin/perl

  use strict;

  use Getopt::Std;
  use Getopt::Long;
  use Pod::Usage;
  use SMAPLIST;
  use ERROR::Handler;

  my $eh = ERROR::Handler->new();

  my %options = ( SILENT=>1 );
  GetOptions ("define=s" => \%options);
  $options{BLACKLIST} = $options{BLACKLIST} // $ENV{SMAP_BLACKLIST};

  my $sl = SMAPLIST->new(%options);
  exit 1 if $eh->isError();

  my @files = $sl->blacklist("Radiometer", @ARGV);

  for my $file (@files) { print "$file\n" }

exit(0);
