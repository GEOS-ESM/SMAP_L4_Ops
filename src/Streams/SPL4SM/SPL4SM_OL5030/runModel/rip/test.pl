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

  my $cfg = CONFIG->copy(%$config);
  
  foreach my $type (keys %$transaction) {
    my $dtr = $transaction->{$type};
    $cfg    = CONFIG->copy(%$cfg, %$dtr);
  }

  my $t = CLOCK->new(%$cfg);
  $t += 86400;

  $cfg->{DATE2} = $t->strftime("%Y%m%d");
  $cfg->{TIME2} = $t->strftime("%H%M%S");

  my ($fh_in, $fh_out);

  my @files = (
             'CAP.rc',
             'cap_restart',
             'HISTORY.rc',
             'LDAS.rc',
             'LDASsa_DEFAULT_inputs_adapt.nml',
             'LDASsa_DEFAULT_inputs_catbias.nml',
             'LDASsa_DEFAULT_inputs_ensprop.nml',
             'LDASsa_DEFAULT_inputs_ensupd.nml',
             'LDASsa_SPECIAL_inputs_catbias.nml',
             'LDASsa_SPECIAL_inputs_ensprop.nml',
             'LDASsa_SPECIAL_inputs_ensupd.nml'
            );

  foreach my $file (@files) {

      open $fh_in, "<$file"; open $fh_out, ">../tmp/$file";
      $cfg->sed($fh_in,$fh_out);
      close $fh_in; close $fh_out;
  }

exit 0;
