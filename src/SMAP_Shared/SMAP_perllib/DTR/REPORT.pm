package DTR::REPORT;

  use strict;

  use overload "+" => \&sum;

  use File::Basename;
  use File::Path;
  use File::Spec;

  use CONFIG;

  use DTR;
  use DTR::OBJECT;

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

#******************************************************************************
sub new { INVOCANT: my $invocant = shift;
          DTR_REF:  my $dtr = shift;
          HASH:     my %options = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name:
# -------------
#
# Purpose:
# --------
#
# Language: Perl
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           mm/dd/ccyy      J.Ardizzone  created.
#******************************************************************************

  my $class = ref($invocant) || $invocant;

  my %self = ();

  $self{GRANULES}        = [];
  $self{PAN_WAITING}     = 0;
  $self{PDR_COUNT}       = 0;
  $self{PAN_COUNT}       = 0;
  $self{PAN_SUCCESS}     = 0;
  $self{PDR_LATENCY}     = 0;
  $self{PAN_LATENCY}     = 0;
  $self{PDR_MIN_LATENCY} = undef;
  $self{PAN_MIN_LATENCY} = undef;
  $self{PDR_MAX_LATENCY} = 0;
  $self{PAN_MAX_LATENCY} = 0;
  $self{DATA_MIN_TIME}   = undef;
  $self{DATA_MAX_TIME}   = undef;
  $self{DATA_BYTES}      = 0;
  $self{DISQUALIFIED}    = 0;

  $dtr or return bless(\%self,$class);

  $self{DTR} = $dtr;

  $dtr->{PDR_REMOTE_LOGIN} =~ /(.*):\/\/(.*)@(.*)/;

  $self{PROTOCOL} = $1;
  $self{CLIENT}   = $2 // "local";
  $self{SERVER}   = $3 // "local";

  $self{CLIENT}   = $dtr->{PDR_EXPORT_CLIENT} // $self{CLIENT};
  $self{PDR_DIR}  = $dtr->{PDR_LOCAL_DIR};
  $self{PAN_DIR}  = $dtr->{PAN_EXPORT_DIR} // $dtr->{PAN_LOCAL_DIR};
  $self{PDRD_DIR} = $dtr->{PDRD_EXPORT_DIR} // $dtr->{PDRD_LOCAL_DIR};

  my $self = bless(\%self,$class);

# Locate PDRs and create report.
# ------------------------------

  $self->report($dtr,%options);

  return $self;
}

#******************************************************************************
sub report { SELF_REF: my $self = shift;
             DTR_REF:  my $dtr = shift;
             HASH:     my %options = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Report
# -------------
#
# Purpose: Creates a data transaction report consisting of PDR statistics.
# -------- PDRs are located using the supplied search options.
#
# Language: Perl
# ---------
#
# See Also: stat()
# ---------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $self            DTR::REPORT    INOUT  self reference
#
# $dtr                     DTR       IN  data transaction
#
# %options                HASH       IN  reporting options
#
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           07/20/2014      J.Ardizzone  created.
#******************************************************************************

  my $clock = CLOCK->new();
  my ($date, $time, $ctime);
  my ($t_prod_min, $t_prod_max);
  my $method = $dtr->{CLOCK_METHOD};

# Retrieve PDRs
# =============

  my @pdr = $dtr->search(%options);

  foreach my $pdr (@pdr) {

    -s $pdr or next;

#   Retrieve the posting time
#   of the PDR (default: file
#   creation time).
#   =========================

    $ctime = (stat($pdr))[9];
    my $t_pdr = gmtime($ctime);

    $pdr =~ /^.*(\d{8})(\d{6})/;

    $date = $1 + 0; $time = $2;
    $t_pdr = CLOCK->new(DATE=>$date, TIME=>$time) if $date;

#   Retrieve the product time
#   of the PDR (default: posting time)
#   ==================================

    my $data_time = $clock->$method($pdr,DATETIME=>1);

    my $t_prod = $data_time // $t_pdr;
    ++$self->{DISQUALIFIED} if $t_prod == $t_pdr;

    $t_prod_min = $t_prod_min // $data_time;
    $t_prod_max = $t_prod_max // $data_time;

    $data_time and do {

      $t_prod_min = $data_time < $t_prod_min ? $data_time : $t_prod_min;
      $t_prod_max = $data_time > $t_prod_max ? $data_time : $t_prod_max;

    };
 
    $self->{DATA_MIN_TIME} = $t_prod_min;
    $self->{DATA_MAX_TIME} = $t_prod_max;

#   Update statistics
#   =================

    $self->{PDR_COUNT}++;

    my $pan = basename $pdr;
    $pan =~ s/\.PDR/\.PAN/;
    $pan = File::Spec->catfile($self->{PAN_DIR}, $pan);

    -f $pan and do {

      my $success = ! system "grep -q SUCCESS $pan";

      $self->{PAN_COUNT}++;
      $self->{PAN_SUCCESS}++ if $success;

#     Note: The PAN time can also
#     be extracted from the PAN file.
#     Using the creation time for simplicity.
#     ---------------------------------------

      $ctime = (stat($pan))[9];
      my $t_pan = gmtime($ctime);

#     Compute latencies
#     -----------------

      $self->latency($t_pdr,$t_pan,$t_prod);

#     Accumulate a list of acquired granules.
#     ---------------------------------------

      $self->pushGranules($pdr);

      next;

    };

    $self->{PAN_WAITING}++;

  }

  return 1;

}

sub latency { SELF:  my $self = shift;
              CLOCK: my $t_pdr = shift;
              CLOCK: my $t_pan = shift;
              CLOCK: my $t_prod = shift;

  my $latency;

# PDR latency is the production
# latency.
# =============================

  $latency                  = $t_pdr->epoch - $t_prod->epoch;

  $self->{PDR_MIN_LATENCY}  = $self->{PDR_MIN_LATENCY} // $latency;
  $self->{PDR_MIN_LATENCY}  = $latency if $latency <= $self->{PDR_MIN_LATENCY};
  $self->{PDR_MAX_LATENCY}  = $latency if $latency >= $self->{PDR_MAX_LATENCY};
  $self->{PDR_LATENCY}     += $latency;

# PAN latency is the acquisition
# latency.
# ==============================

  $latency                  = $t_pan->epoch - $t_pdr->epoch;

  $self->{PAN_MIN_LATENCY}  = $self->{PAN_MIN_LATENCY} // $latency;
  $self->{PAN_MIN_LATENCY}  = $latency if $latency <= $self->{PAN_MIN_LATENCY};
  $self->{PAN_MAX_LATENCY}  = $latency if $latency >= $self->{PAN_MAX_LATENCY};
  $self->{PAN_LATENCY}     += $latency;

  return 1;
}

sub pushGranules { SELF:   my $self = shift;
                   SCALAR: my $pdr = shift;

  my $dtr = $self->{DTR};
  my $config = CONFIG->new();

  my $granule = $self->{GRANULES} // [];
  my @file = $config->configure($pdr);
  my @objects = $dtr->getObjects(@file);

  foreach my $obj (@objects) {
    push @$granule, File::Spec->catfile($obj->{DIRECTORY_ID},
                                                 $obj->{FILE_ID});
    $self->{DATA_BYTES} += $obj->{FILE_SIZE};
  }

  return 1;
}

sub stat { SELF_REF: my $self = shift;

    use integer;

    my $pan_failed = $self->{PAN_COUNT} - $self->{PAN_SUCCESS};

    my ($t, $days, $hours, $min);

#   PDR Statistics
#   ==============

    $days = $self->{PDR_MIN_LATENCY} / 86400;
    $hours = ($self->{PDR_MIN_LATENCY} % 86400) / 3600;
    $min   = (($self->{PDR_MIN_LATENCY} % 86400) % 3600) / 60;

    my $pdr_min_latency = [ $days, $hours, $min ];

    $days = $self->{PDR_MAX_LATENCY} / 86400;
    $hours = ($self->{PDR_MAX_LATENCY} % 86400) / 3600;
    $min   = (($self->{PDR_MAX_LATENCY} % 86400) % 3600) / 60;

    my $pdr_max_latency = [ $days, $hours, $min ];

    my $pdr_latency = 0;
    my $pdr_count   = $self->{PDR_COUNT} - $self->{DISQUALIFIED};

    $pdr_latency = $self->{PDR_LATENCY} / $pdr_count if $pdr_count;
    $days = $pdr_latency / 86400;
    $hours = ($pdr_latency % 86400) / 3600;
    $min   = (($pdr_latency % 86400) % 3600) / 60;

    my $pdr_mean_latency = [ $days, $hours, $min ];

#   PAN Statistics
#   ==============

    $days = $self->{PAN_MIN_LATENCY} / 86400;
    $hours = ($self->{PAN_MIN_LATENCY} % 86400) / 3600;
    $min   = (($self->{PAN_MIN_LATENCY} % 86400) % 3600) / 60;

    my $pan_min_latency = [ $days, $hours, $min ];

    $days = $self->{PAN_MAX_LATENCY} / 86400;
    $hours = ($self->{PAN_MAX_LATENCY} % 86400) / 3600;
    $min   = (($self->{PAN_MAX_LATENCY} % 86400) % 3600) / 60;

    my $pan_max_latency = [ $days, $hours, $min ];

    my $pan_latency = 0;
    $pan_latency = $self->{PAN_LATENCY} / $self->{PAN_COUNT}
                                           if $self->{PAN_COUNT};
    $days = $pan_latency / 86400;
    $hours = ($pan_latency % 86400) / 3600;
    $min   = (($pan_latency % 86400) % 3600) / 60;

    my $pan_mean_latency = [ $days, $hours, $min ];

#   Return results
#   ==============

    return  ($self->{PDR_COUNT},
             $pdr_min_latency, $pdr_max_latency, $pdr_mean_latency,

             $self->{PAN_COUNT},
             $pan_min_latency, $pan_max_latency, $pan_mean_latency,

             $pan_failed, $self->{GRANULES},

             $self->{DATA_MIN_TIME}, $self->{DATA_MAX_TIME}, 

             $self->{DATA_BYTES});
}


sub sum { REPORT_REF: my $report1 = shift;
          REPORT_REF: my $report2 = shift;

  my %report = ();
  my ($min1, $min2);
  my ($max1, $max2);
  my $class = ref($report1);
  my $granules1 = $report1->{GRANULES} // [];
  my $granules2 = $report2->{GRANULES} // [];

  $report{DTR}      = $report1->{DTR}      // $report2->{DTR};
  $report{PROTOCOL} = $report1->{PROTOCOL} // $report2->{PROTOCOL};
  $report{CLIENT}   = $report1->{CLIENT}   // $report2->{CLIENT};
  $report{SERVER}   = $report1->{SERVER}   // $report2->{SERVER};

  $report{PDR_DIR}      = $report1->{PDR_DIR}      // $report2->{PDR_DIR};
  $report{PAN_DIR}      = $report1->{PAN_DIR}      // $report2->{PAN_DIR};
  $report{PDRD_DIR}     = $report1->{PDRD_DIR}     // $report2->{PDRD_DIR};
  $report{CLOCK_METHOD} = $report1->{CLOCK_METHOD} // $report2->{CLOCK_METHOD};

  $report{GRANULES} = [ @$granules1, @$granules2 ];

  $report{PAN_WAITING}  = $report1->{PAN_WAITING}  + $report2->{PAN_WAITING};
  $report{PDR_COUNT}    = $report1->{PDR_COUNT}    + $report2->{PDR_COUNT};
  $report{PAN_COUNT}    = $report1->{PAN_COUNT}    + $report2->{PAN_COUNT};
  $report{PAN_SUCCESS}  = $report1->{PAN_SUCCESS}  + $report2->{PAN_SUCCESS};
  $report{PDR_LATENCY}  = $report1->{PDR_LATENCY}  + $report2->{PDR_LATENCY};
  $report{PAN_LATENCY}  = $report1->{PAN_LATENCY}  + $report2->{PAN_LATENCY};
  $report{DISQUALIFIED} = $report1->{DISQUALIFIED} + $report2->{DISQUALIFIED};

  $report{DATA_BYTES}   = $report1->{DATA_BYTES}   + $report2->{DATA_BYTES};

# Compute the min/max PDR latency
# (AKA: Production latency)
# ===============================

  $min1 = $report1->{PDR_MIN_LATENCY};
  $min2 = $report2->{PDR_MIN_LATENCY};

  if ($min1 and $min2) {

    $report{PDR_MIN_LATENCY} = $min1 < $min2 ? $min1 : $min2;

  } else {

    $report{PDR_MIN_LATENCY} = $min1 || $min2;
    $report{PDR_MIN_LATENCY} = $report{PDR_MIN_LATENCY} || undef;

  }
    
  $report{PDR_MAX_LATENCY} =
  ($report1->{PDR_MAX_LATENCY} > $report2->{PDR_MAX_LATENCY}) ?
  $report1->{PDR_MAX_LATENCY} : $report2->{PDR_MAX_LATENCY};

# Compute the min/max PAN latency
# (AKA: Acquisition latency)
# ===============================

  $min1 = $report1->{PAN_MIN_LATENCY};
  $min2 = $report2->{PAN_MIN_LATENCY};

  if ($min1 and $min2) {

    $report{PAN_MIN_LATENCY} = $min1 < $min2 ? $min1 : $min2;

  } else {

    $report{PAN_MIN_LATENCY} = $min1 || $min2;
    $report{PAN_MIN_LATENCY} = $report{PAN_MIN_LATENCY} || undef;

  }

  $report{PAN_MAX_LATENCY} =
  ($report1->{PAN_MAX_LATENCY} > $report2->{PAN_MAX_LATENCY}) ?
  $report1->{PAN_MAX_LATENCY} : $report2->{PAN_MAX_LATENCY};

# Compute the min/max data time
# =============================

  $min1 = $report1->{DATA_MIN_TIME};
  $min2 = $report2->{DATA_MIN_TIME};

  if ($min1 and $min2) {

    $report{DATA_MIN_TIME} = $min1 < $min2 ? $min1 : $min2;

  } else {

    $report{DATA_MIN_TIME} = $min1 || $min2;
    $report{DATA_MIN_TIME} = $report{DATA_MIN_TIME} || undef;

  }

  $max1 = $report1->{DATA_MAX_TIME};
  $max2 = $report2->{DATA_MAX_TIME};

  if ($max1 and $max2) {

    $report{DATA_MAX_TIME} = $max1 > $max2 ? $max1 : $max2;

  } else {

    $report{DATA_MAX_TIME} = $max1 || $max2;
    $report{DATA_MAX_TIME} = $report{DATA_MAX_TIME} || undef;

  }

# my $t_prod_min = $report{DATA_MIN_TIME};
# my $t_prod_max = $report{DATA_MAX_TIME};
# print "Min time: ", $t_prod_min->strftime("%Y%m%dT%H%M%S"), "\n";
# print "Max time: ", $t_prod_max->strftime("%Y%m%dT%H%M%S"), "\n";

  return bless(\%report,$class);

}

1;
