package L4_SM::LMC::QA;
our @ISA = "L4_SM::QA";

use strict;

use L4_SM::QA;
use File::Basename;
use File::Path;
use File::Spec;

sub addMeta()
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  HASH_PTR: my $config = scalar(@_) ? shift : {};

  return %$config;
}

1;
