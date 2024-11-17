                        #############################
                        #                           #
                        # PACKAGE SYSTEM::SCHEDULER #
                        #                           #
                        #############################

package SYSTEM::SCHEDULER;

use strict;

use File::Basename;
use File::Path;
use File::Spec;

#******************************************************************************
sub new
#******************************************************************************
{

# Argument List
# -------------

  my $invocant = shift;
  my %config   = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  my $self   = \%config;
  my $class = ref($invocant) || $invocant;

  bless($self, $class);
}

sub isEnabled
{

  my $self = shift;

  return $self->{ENABLED};
}

sub openProcess
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

# Take no action if the process is
# disabled or one is already open.
# ================================

  $self->isEnabled() or return 0;
  return 1 if $self->isOpenProcess();

# Create the process monitoring file
# if one doesn't exist.
# ==================================

  if ( ! -f $file ) { open $fh, ">$file"; close $fh; chmod 0100, $file }

# Set the bit flag for the first or
# second attempt. Return false after
# the second attempt.
# ==================================

  ($mode) = (stat($file))[2];

  return 0 if $mode & 0001;

  if ($mode & 0010) { $mode = $mode | 0001; chmod $mode, $file; return 1 }

  $mode = $mode | 0010; 
  chmod $mode, $file;

  return 1;

}

sub isReady
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  return 1 if ! -f $file;
  $self->isEnabled() or return 0;

  ($mode) = (stat($file))[2];

  return 0 if ($mode & 0077);
  return 1 if ($mode & 0100);

  return 0;
}

sub closeProcess
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  return 1 if ! -f $file;
  $self->isEnabled() or return 0;

  chmod 0100, $file;

  return 1;
}

sub isFinished
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  return 0 if ! -f $file;
  $self->isEnabled() or return 0;

  ($mode) = (stat($file))[2];

  return $mode & 0006 if $mode & 0001;
  return $mode & 0060 if $mode & 0010;

  return 0;
}

sub isaSuccess
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  return 0 if ! -f $file;
  $self->isEnabled() or return 0;

  ($mode) = (stat($file))[2];

  return $mode & 0004 if $mode & 0001;
  return $mode & 0040 if $mode & 0010;

  return 0;
}

sub isOpenProcess
{

# Argument List
# -------------

  REFERENT: my $self = shift;

# Local Variables
# ---------------

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  return 0 if ! -f $file;
  $self->isEnabled() or return 0;

  ($mode) = (stat($file))[2];

  return 0 if $mode & 0006;
  return 1 if $mode & 0001;

  return 0 if $mode & 0060;
  return 1 if $mode & 0010;

}

sub setStatus
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  HASH:     my %options = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;

  INTEGER: my $mode;
  STRING:  my $path = $self->{PATH};
  STRING:  my $file = File::Spec->catfile($path, ".status");

  if ( ! -f $file ) { open $fh, ">$file"; close $fh; chmod 0100, $file }

  ($mode) = (stat($file))[2];

  foreach (keys %options) {

    /SUCCESS/     and do { $mode = $mode | 0004 if $mode & 0001;
                           chmod $mode, $file if $mode & 0004;

                           $mode = $mode | 0040 if $mode & 0010;
                           chmod $mode, $file if $mode & 0040;

                           return 1;
                         };

    /FAILED/      and do { $mode = $mode | 0002 if $mode & 0001;
                           chmod $mode, $file if $mode & 0002;

                           $mode = $mode | 0020 if $mode & 0010;
                           chmod $mode, $file if $mode & 0020;

                           return 1;
                         };

    /ENABLE/      and do { $mode = $mode | 0100;
                           chmod $mode, $file;

                           return 1;
                         };

    /DISABLE/     and do { $mode = $mode & 0077;
                           chmod $mode, $file;

                           return 1;
                         };

    /CLEAR/       and do { chmod 0100, $file;
                           return 1;
                         };

  }

  return 0;

}

1;
