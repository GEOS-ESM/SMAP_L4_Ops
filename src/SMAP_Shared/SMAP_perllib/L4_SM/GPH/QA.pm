package L4_SM::GPH::QA;
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

  my $hash;
  my $omission;
  my ($key, $value);

# Number of EASEv2 grid points
# ============================

  $key = "Number of L4_SM EASEv2  9 km land grid cells";
  my $num_grid_cells_9km = $self->{$key} // 0;

# sm_surface_wetness Completeness Omission Value
# ==============================================

  $hash = $self->{sm_surface_wetness};
  $value = $hash->{COUNT} // 0;

  $omission = 1.0 - ($value / $num_grid_cells_9km);
  $config->{SRF_WET_COMPLETENESS_OMISSION} = $omission * 100;

# sm_rootzone_wetness Completeness Omission Value
# ===============================================

  $hash = $self->{sm_rootzone_wetness};
  $value = $hash->{COUNT} // 0;

  $omission = 1.0 - ($value / $num_grid_cells_9km);
  $config->{SRF_RZ_WET_COMPLETENESS_OMISSION} = $omission * 100;

# sm_profile_wetness Completeness Omission Value
# ==============================================

  $hash = $self->{sm_profile_wetness};
  $value = $hash->{COUNT} // 0;

  $omission = 1.0 - ($value / $num_grid_cells_9km);
  $config->{SRF_PRF_WET_COMPLETENESS_OMISSION} = $omission * 100;

# sm_rootzone Completeness Omission Value
# =======================================

  $hash = $self->{sm_rootzone};
  $value = $hash->{COUNT} // 0;

  $omission = 1.0 - ($value / $num_grid_cells_9km);
  $config->{SRF_RZ_COMPLETENESS_OMISSION} = $omission * 100;

# Save the new metadata and return.
# =================================

  foreach $key (keys %$config) { $self->{$key} = $config->{$key} };

  return %$config;
}

1;
