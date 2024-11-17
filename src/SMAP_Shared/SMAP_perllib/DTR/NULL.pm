                        #######################
                        #                     #
                        #  PACKAGE DTR::NULL  #
                        #                     #
                        #######################

package DTR::NULL;
our @ISA = "DTR::TRANSACTION";

use strict;
use DTR::TRANSACTION;

sub list { return () }
sub get { return 1 }
sub put { return 1 }

1;
