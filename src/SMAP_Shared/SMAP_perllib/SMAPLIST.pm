                        #######################
                        #                     #
                        #  PACKAGE SMAPLIST   #
                        #                     #
                        #######################

package SMAPLIST;

use strict;

use File::Basename;
use File::Path;
use File::Spec;

use LOG::Handler;
use ERROR::Handler;

sub new
{

# Argument List
# =============

  my $invocant = shift;
  my %options  = scalar(@_) ? @_ : ();

  my $self  = \%options;
  my $class = ref($invocant) || $invocant;
  my $obj   = bless($self,$class);

  $self->{VERBOSE} = $self->{VERBOSE} // 1;
  $self->{VERBOSE} = $self->{SILENT} ? 0 : $self->{VERBOSE};

  $obj->readBlackList($options{BLACKLIST}) if $options{BLACKLIST};

  return $obj;
}

sub readBlackList { my $self = shift;
                    my $pathname = shift;

  my $this = "SMAPLIST::readBlackList";
  my $eh   = ERROR::Handler->new(\&error_handler,$this);

  my %blacklist = ();
  $self->{BLACKLIST} = \%blacklist;

  -f $pathname or return $eh->error(1,FILE=>$pathname);

  open my $fh, "<$pathname";
  local $/ = undef;
  my $content = <$fh>;
  my @lines = split /\r\n|\n|\r/, $content;
  close $fh;

  for (@lines) {

    /HALF ORBIT/ or do {

      my @fields = (split /,/);
      my $crid   = $fields[1];
      my $type   = $fields[2];
      my $stream = $fields[3];

      $stream =~ /\w+/ or next;

      my ($orbit1, $orbit2) = (split /-/, $fields[0]);

      $orbit2 = $orbit2 // $orbit1;

      $orbit1 =~ /(\d+)([AD])/;
      $orbit1 = sprintf("%05d", $1) . "_" . $2;

      $orbit2 =~ /(\d+)([AD])/;
      $orbit2 = sprintf("%05d", $1) . "_" . $2;

      while ($orbit1 le $orbit2) {

          my $hash = $blacklist{$stream} // {};
          $blacklist{$stream} = $hash;

          my $id = $orbit1;
          $id = $orbit1 . "_" . $crid if $crid;

          $hash->{$id} = $type;

          $orbit1 =~ /^0*(\d+)_([AD])/;
          my $number = $1;
          my $letter = ($2 eq "A") ? "D" : "A";

          $number += 1 if $letter eq "A";
          $orbit1 = sprintf("%05d", $number) . "_" . $letter;
      }

    };

  }

  my $size = keys %blacklist;
  $size == 0 and return $eh->warning(2,FILE=>$pathname);

}

sub blacklist { my $self  = shift;
                my $type  = shift;
                my @files = scalar(@_) ? sort @_ : ();

  my $this = "SMAPLIST::blacklist";
  my $eh   = ERROR::Handler->new(\&error_handler,$this);

  my %list      = ();

  my $min_crid  = $self->{MIN_CRID} // 0;
  my $max_crid  = $self->{MAX_CRID} // 99999;
  $min_crid     = $1 if $min_crid =~ /[A-Z](\d{5})/;
  $max_crid     = $1 if $max_crid =~ /[A-Z](\d{5})/;

  my $blist     = $self->{BLACKLIST} || {};
  $blist        = $blist->{$type} // {};

  foreach my $filename (@files) {

    my $id              = "unknown";
    my $orbit_id        = "unknown";
    my $version_id      = undef;
    my $last_version_id = undef;
    my $crid            = 0;

    $filename =~ /SMAP_.+(\d{5}_[DA])_.+_([A-Z])(\d{5})_(\d{3})/ and do {

      $orbit_id   = $1;
      $version_id = $2 . $3 . "_" . $4;
      $crid       = $3;
      $id         = $1 . "_" . $2 . $3;

    };

    next if $crid < $min_crid or $crid > $max_crid;

    ($blist->{$id} or $blist->{$orbit_id}) and do {
      $self->{VERBOSE} and $eh->advisory(1,FILE=>$filename);
      next;
    };

    $last_version_id = $1 if $list{$orbit_id} =~ /SMAP_.+([A-Z]\d{5}_\d{3})/;

    $list{$orbit_id} = $filename if $version_id gt $last_version_id;

  }

  return sort values %list;
}

sub error_handler { my $error_handle = shift;
                    my $error_code = shift;
                    my %options = scalar(@_) ? @_ : ();

  my $type = $options{ERROR_TYPE};
  my $lh   = LOG::Handler->new(HANDLE=>$error_handle);

# Do not print elapsed time for
# this module.
# =============================

  defined $options{ERROR_ELAPSED_TIME} and return;

# Trap traceback event
# ====================

  $options{ERROR_TRACEBACK} and do {

    $lh->error($error_code,"Traceback: $error_handle() encountered an error.");
    return;

  };

# Comments have a specified
# return code of 0.
# =========================

  $error_code or return $lh->comment(0,$options{COMMENT});

# blacklist() error handle
# ========================

  $error_handle eq "SMAPLIST::blacklist" and do {

    $error_code == 1 and do {
      $lh->$type("SML-001","The following granule was blacklisted: " .
                                                     "\"$options{FILE}\"");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

# readBlackList() error handle
# ========================

  $error_handle eq "SMAPLIST::readBlackList" and do {

    $error_code == 1 and do {
      $lh->$type("SML-002","No such file: \"$options{FILE}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type("SML-003","No entries found in : \"$options{FILE}\"");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

  print STDERR "No error handle for \"$error_handle\"\n";

  return;

}

1;
