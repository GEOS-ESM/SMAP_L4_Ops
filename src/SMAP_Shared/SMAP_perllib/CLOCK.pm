package CLOCK;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use Time::Piece;
use Time::Local;

our @ISA = "Time::Piece";

sub new {

  use integer;

# Argument List
# -------------

  my $invocant = shift;
  my %config   = scalar(@_) ? @_ : ();

  my $self     = gmtime();
  my $class    = ref($invocant) || $invocant;

  my $date = $self->year * 10000 + $self->mon * 100 + $self->mday;
  my $time = $self->hour * 10000 + $self->min * 100 + $self->sec;

  $date = $config{DATE} if defined $config{DATE};
  $time = $config{TIME} if defined $config{TIME};

  my $year  = $date / 10000;
  my $month = ($date - ($year * 10000) ) / 100;
  my $day   = $date - ($year * 10000) - ($month * 100);

  my $hour = $time / 10000;
  my $minute = ($time - ($hour * 10000) ) / 100;
  my $second = $time - ($hour * 10000) - ($minute * 100);

  my $epoch = timegm($second,$minute,$hour,$day,$month-1,$year);

  $self = gmtime($epoch);

  return bless($self, $class);

}

sub sort { my $self = shift;
           my $list = shift;
           my %options = scalar(@_) ? @_ : ();

  my $method = $options{CLOCK_METHOD};

  $method or do { return reverse sort @$list if $options{REVERSE};
                  return sort @$list;
                };

# Bin elements by time
# ====================

  my %hash;

  foreach my $index (0..$#{$list}) {

    my $time = $self->$method($list->[$index],DATETIME=>1);
    my $t    = $time ? $time->epoch : -1;

    my $array = $hash{$t} // [];
    push @$array, $index;
    $hash{$t} = $array;

  }

# Sort elements by time
# =====================

  my @list;

  foreach my $key (sort keys %hash) {

    my @array = @{$hash{$key}};

    if ($options{UNIQUE}) {  push @list, $list->[$array[-1]]; next }
    push @list, @$list[@array];

  }

# Return time sorted elements
# ===========================

  return reverse @list if $options{REVERSE};

  return @list;

}
  
sub timeless
{

  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  if ($options{DATETIME}) { return undef }

  return 1;
}

sub smap_old_time
{

  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my @fileID;
  my ($date, $time);

  open FILE, "<$pathname";
  @fileID = grep /^\s*FILE_ID\s*=/i, <FILE>;
  close FILE;

  foreach (@fileID) { 

    /(\d{8})T(\d{6})/   || next;

    $date = $1; $time = $2;

    $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

    $date = $self->strftime("%Y%m%d");

    /$date/             && return 1;

  }

  return undef;
}

sub smap_l3_time
{

  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my @fileID;
  my ($date, $time);

  open FILE, "<$pathname";
  @fileID = grep /^\s*FILE_ID\s*=/i, <FILE>;
  close FILE;

  foreach (@fileID) {

    /_(\d{8})_/   || next;

    $date = $1; $time = 0;

    $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

    $date = $self->strftime("%Y%m%d");

    /$date/             && return 1;

  }

  return undef;
}

sub smap_time
{
  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my ($date, $time);

  $pathname =~ /(\d{8})T(\d{6})/ || return undef;

  $date = $1; $time = $2;

  $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

  $date = $self->strftime("%Y%m%d");

  $pathname =~ /$date/ && return 1;

  return undef;
}

sub smos_time
{
  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my ($date, $time);

  $pathname =~ /(\d{8})(\d{4})/ || return undef;

  $date = $1; $time = $2 . "00";

  $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

  $date = $self->strftime("%Y%m%d");

  $pathname =~ /$date/ && return 1;

  return undef;
} 

sub modis_time
{
  REFERENT: my $self = shift;
  STRING:   my $pathname = shift;
  HASH:     my %options = scalar(@_) ? @_ : ();

  STRING:  my $name = basename $pathname;
  INTEGER: my ($date, $time, $year, $julian);

  $name =~ /A(\d{4})(\d{3}).+\d{7}(\d{6})/ || return undef;

  $year     = $1;
  $julian   = $2;
  $date     = $year . "0101";
  $time     = $3;

  $options{DATETIME} and
     return CLOCK->new(DATE=>$date,TIME=>$time) + ($julian - 1) * 86400;

  $date = "A" . $self->strftime("%Y%j");

  $pathname =~ /$date/ && return 1;

  return undef;
}

sub iso_time
{
  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my ($date, $time);

  $pathname =~ /(\d{8})T(\d{6})/ || return undef;

  $date = $1; $time = $2;

  $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

  $date = $self->strftime("%Y%m%d");

  $pathname =~ /$date/ && return 1;

  return undef;
}

sub smap_daily
{
  my $self = shift;
  my $pathname = shift;
  my %options = scalar(@_) ? @_ : ();

  my ($date, $time);

  $pathname =~ /_(\d{8})_/ || return undef;

  $date = $1; $time = 0;

  $options{DATETIME}  && return CLOCK->new(DATE=>$date,TIME=>$time);

  $date = $self->strftime("%Y%m%d");

  $pathname =~ /$date/ && return 1;

  return undef;
}

1;
