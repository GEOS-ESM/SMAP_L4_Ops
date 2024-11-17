                        ##############################
                        #                            #
                        #  PACKAGE BATCH::SYSTEM     #
                        #                            #
                        ##############################

package SYSTEM::BATCH;

use strict;

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

1;
