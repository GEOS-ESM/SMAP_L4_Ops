package L4_SM::AUP::QA;
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
  my ($key, $value);

# Number of EASEv2 grid points
# ============================

  $key = "Number of L4_SM EASEv2 36 km land grid cells";
  my $num_grid_cells_36km = $self->{$key} // 110772;

  $key = "Number of L4_SM EASEv2  9 km land grid cells";
  my $num_grid_cells_9km = $self->{$key} // 0;

# TB_H OBS Completeness Omission Value
# ====================================

  $hash = $self->{tb_h_obs_36km};
  my $num_tb_h_obs_36km = $hash->{COUNT} // 0;

  $hash = $self->{tb_h_obs_09km_D};
  my $num_tb_h_obs_9km = $hash->{COUNT} // 0;

  $value = (($num_tb_h_obs_36km / $num_grid_cells_36km) + 
                  ($num_tb_h_obs_9km / $num_grid_cells_9km)) * 100.0; 

  $config->{TBH_OBS_COMPLETENESS_OMISSION} = 100.0 - $value;

# TB_H Forecast Completeness Omission Value
# =========================================

  $hash = $self->{tb_h_forecast_36km};
  my $num_tb_h_forecast_36km = $hash->{COUNT} // 0;

  $hash = $self->{tb_h_forecast_09km_D};
  my $num_tb_h_forecast_9km = $hash->{COUNT} // 0;

  $value = (($num_tb_h_forecast_36km / $num_grid_cells_36km) +
                  ($num_tb_h_forecast_9km / $num_grid_cells_9km)) * 100.0;

  $config->{TBH_FC_COMPLETENESS_OMISSION} = 100.0 - $value;

# TB_V Completeness Omission Value
# ================================

  $hash = $self->{tb_v_obs_36km};
  my $num_tb_v_obs_36km = $hash->{COUNT} // 0;

  $hash = $self->{tb_v_obs_09km_D};
  my $num_tb_v_obs_9km = $hash->{COUNT} // 0;

  $value = (($num_tb_v_obs_36km / $num_grid_cells_36km) +
                  ($num_tb_v_obs_9km / $num_grid_cells_9km)) * 100.0;

  $config->{TBV_OBS_COMPLETENESS_OMISSION} = 100.0 - $value;

# TB_V Forecast Completeness Omission Value
# =========================================

  $hash = $self->{tb_v_forecast_36km};
  my $num_tb_v_forecast_36km = $hash->{COUNT} // 0;

  $hash = $self->{tb_v_forecast_09km_D};
  my $num_tb_v_forecast_9km = $hash->{COUNT} // 0;

  $value = (($num_tb_v_forecast_36km / $num_grid_cells_36km) +
                  ($num_tb_v_forecast_9km / $num_grid_cells_9km)) * 100.0;

  $config->{TBV_FC_COMPLETENESS_OMISSION} = 100.0 - $value;

 foreach $key (keys %$config) { $self->{$key} = $config->{$key} };

  return %$config;
}

1;
