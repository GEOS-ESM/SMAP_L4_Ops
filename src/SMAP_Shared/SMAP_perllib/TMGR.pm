                        #######################
                        #                     #
                        #     PACKAGE TMGR    #
                        #                     #
                        #######################

package TMGR;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use CLOCK;

#******************************************************************************
sub new
#******************************************************************************
{
# Argument List
# -------------

  INVOCANT: my $invocant = shift;
  STRING:   my $path = scalar(@_) ? shift : undef;

# Local Variables
# ---------------

  HASH:        my %self = ();
  CLOCK:       my ($t, $tend);
  STRING:      my ($root, $file, $clock, $line, @dirs);
  STRING:      my $class = ref($invocant) || $invocant;
  INTEGER:     my $n;
  INTEGER:     my ($date, $time, $edate, $etime);
  FILE_HANDLE: my $fh;

# Return an empty hash referent 
# if no path was specified.
# =============================

  $path or return bless(\%self, $class);
  $self{PATH} = $path;

# Retrieve a list of 
# directories from the path.
# ==========================

  @dirs = File::Spec->splitdir( $path );

# Traverse the path from child to ancestor
# to locate the first occurrence of a clock
# file. The clock will be the ruling time
# keeper for any processes located lower in
# the directory hierarchy. The oldest ancestor
# is the directory containing the empty file
# named, ".root".
# =========================================

  foreach $n (reverse (0..$#dirs)) {

    $root  = File::Spec->catdir(@dirs[0..$n], ".root");
    $clock = File::Spec->catdir(@dirs[0..$n], "clock");

    last if -s $clock;
    last if -f $root;

  }

  $self{CLOCK} = $clock if -s $clock;

# Set the date/time. Use the current date/time
# if no clock exists.
# ============================================

  $t = CLOCK->new();
  $self{DATE} = $t->strftime("%Y%m%d");
  $self{TIME} = $t->strftime("%H%M%S");

  return bless(\%self, $class) if ! -s $clock;

  open $fh, "<$clock"; $line = <$fh>; close $fh;
  ($date, $time, $edate, $etime) = (split /\s+/, $line)[0,1,4,5];

  $t    = CLOCK->new(DATE=>$date, TIME=>$time);
  $tend = CLOCK->new(DATE=>$edate, TIME=>$etime);
  ($self{DATE}, $self{TIME}) = ($date, $time);
  ($self{EDATE}, $self{ETIME}) = ($edate, $etime);

  chmod 0644, $clock if $t->epoch > $tend->epoch;

  bless(\%self, $class);

}

sub updateClock
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  
# Local Variables
# ---------------

  CLOCK:       my ($t, $tstart, $tend);
  STRING:      my $line;
  STRING:      my $clock = $self->{CLOCK};
  INTEGER:     my ($date, $time, $sdate, $stime, $edate, $etime, $incsec);
  FILE_HANDLE: my $fh;

# Return if clock is not running
# (or non-existent).
# ==============================

  $clock or return;
  return if ! $self->isClockRunning();

# Retrieve the date/time from
# the clock file.
# ===========================

  open $fh, "<$clock"; $line = <$fh>; close $fh;
  ($date, $time, $sdate, $stime, $edate, $etime, $incsec) = split /\s+/, $line;

  $t      = CLOCK->new(DATE=>$date,TIME=>$time);
  $tstart = CLOCK->new(DATE=>$sdate,TIME=>$stime);
  $tend   = CLOCK->new(DATE=>$edate,TIME=>$etime);

# Increment the time and update
# the clock.
# ==============================

  $t += $incsec;

  $date = $t->strftime("%Y%m%d");
  $time = $t->strftime("%H%M%S");
  $self->{DATE} = $date;
  $self->{TIME} = $time;

  open $fh, ">$clock"; 
  print $fh "$date $time $sdate $stime $edate $etime $incsec\n";
  close $fh;

# Set the access and modification time
# to the new date/time.
# ====================================

  utime $t->epoch, $t->epoch, "$clock";

# Stop the clock if the new date/time
# has passed the end date/time.
# ===================================

  chmod 0644, $clock if $t->epoch > $tend->epoch;

}

sub tickClock
{

  my $self = shift;
  my $incsec = scalar(@_) ? shift : 60;

  my $t;
  my $fh;
  my $line;
  my $clock = $self->{CLOCK};
  my ($date, $time, $mtime);

  return 0 if ! $clock;
  return 0 if ! $self->isClockRunning();

  open $fh, "<$clock"; $line = <$fh>; close $fh;
  ($date, $time) = (split /\s+/, $line)[0,1];

  $t = CLOCK->new(DATE=>$date,TIME=>$time);
  ($mtime) = (stat($clock))[9];

  $mtime += $incsec;
  utime $mtime, $mtime, $clock;

  return 1;
}

sub elapsedTime
{

  my $self = shift;

  my $t;
  my $fh;
  my $line;
  my $clock = $self->{CLOCK};
  my ($mtime, $date, $time, $elapsed);

  return 0 if ! $clock;

  open $fh, "<$clock"; $line = <$fh>; close $fh;
  ($date, $time) = split /\s+/, $line;

  $t = CLOCK->new(DATE=>$date,TIME=>$time);
  ($mtime) = (stat($clock))[9];

  return $mtime - $t->epoch;
}

sub isExpired
{

  my $self = shift;
  my %options = scalar(@_) ? @_ : ();

  defined $options{WAIT_TIME} or return 0;
  return 1 if $self->elapsedTime() > $options{WAIT_TIME};

  return 0;
}

sub isClockRunning
{

  my $self = shift;

  my $mode;
  my $clock = $self->{CLOCK};

  return 0 if ! $clock;

  ($mode) = (stat($clock))[2];
  return $mode & 0100;
}

sub startClock
{

  my $self = shift;

  my $clock = $self->{CLOCK};
  -s $clock or return 0;

# Retrieve the clock information
# ==============================

  open my $fh, "<$clock"; my $line = <$fh>; close $fh;
  my ($date, $time, $edate, $etime) = (split /\s+/, $line)[0,1,4,5];

  my $t    = CLOCK->new(DATE=>$date, TIME=>$time);
  my $tend = CLOCK->new(DATE=>$edate, TIME=>$etime);
  ($self->{DATE}, $self->{TIME}) = ($date, $time);

# Start the clock if time has
# not expired.
# ============================

  chmod 0644, $clock if $t->epoch > $tend->epoch;
  chmod 0744, $clock if $t->epoch <= $tend->epoch;

  return $self->isClockRunning();
}

sub stopClock
{

  my $self = shift;

  my $clock = $self->{CLOCK};
  -s $clock or return 0;

  chmod 0644, $clock;

  return 1;
}

sub setClock { my $self = shift;
               my %options = scalar(@_) ? @_ : ();

  -d $self->{PATH} or return 0;

  my $today = gmtime();
  my $date  = $options{DATE}  || $today->strftime("%Y%m%d");
  my $sdate = $options{SDATE} || $date;
  my $edate = $options{EDATE} || $sdate;

  my $stime  = $options{STIME} // 0;
  my $etime  = $options{ETIME} // $stime;
  my $incsec = $options{INCSEC} || 86400;

  my $clock = $self->{CLOCK} // File::Spec->catfile($self->{PATH}, "clock");
  $self->{CLOCK} = $clock;

# Set the clock
# =============

  $self->stopClock();

  my $t = CLOCK->new(DATE=>$sdate, TIME=>$stime);

  open my $fh, ">$clock";
  print $fh "$sdate $stime $sdate $stime $edate $etime $incsec\n";
  close $fh;

  utime $t->epoch, $t->epoch, "$clock";

  $self->startClock() or return 0;

  return 1;

}

1;
