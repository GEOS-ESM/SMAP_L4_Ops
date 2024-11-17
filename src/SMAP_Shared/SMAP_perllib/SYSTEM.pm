                        #######################
                        #                     #
                        #   PACKAGE SYSTEM    #
                        #                     #
                        #######################

package SYSTEM;

use strict;

use File::Basename;
use File::Path;
use File::Spec;

use SYSTEM::PBS;

my $IGNORE = 0;

#******************************************************************************
sub new
#******************************************************************************
{
# Argument List
# -------------

  INVOCANT: my $invocant = shift;
  STRING:   my $path = scalar(@_) ? shift : undef;
  HASH:     my %options = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  HASH:        my %self = %options;
  STRING:      my ($root, $file, @dirs);
  STRING:      my $class = ref($invocant) || $invocant;
  INTEGER:     my $n;
  INTEGER:     my $mode = 0;
  INTEGER:     my ($date, $time, $edate, $etime);

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
# to locate the first occurrence of the
# ".root" empty file.
# ========================================

  foreach $n (reverse (0..$#dirs)) {

    $root = File::Spec->catdir(@dirs[0..$n], ".root");
    last if -f "$root";

  }

# Look for disabled processes starting with 
# the root directory. A process is considered
# to be disabled if any parent directory 
# contains a file named, ".status", with the 
# execute file mode bit set to zero for owner.
# ============================================

  $self{ENABLED} = 1;
  $IGNORE = $self{IGNORE} if exists $self{IGNORE};

  foreach $n ($n..$#dirs) {

    $IGNORE and next;

    $file = File::Spec->catdir(@dirs[0..$n], ".status");
    next if ! -f $file;

    ($mode) = (stat($file))[2];
    $self{ENABLED} = $mode & 0100;

    last if ! $self{ENABLED};

  }

  return SYSTEM::PBS->new(%self);

}

1;
