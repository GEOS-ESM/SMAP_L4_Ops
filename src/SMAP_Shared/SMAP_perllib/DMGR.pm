                        #######################
                        #                     #
                        #     PACKAGE DMGR    #
                        #                     #
                        #######################

package DMGR;

use strict;

use Cwd qw(abs_path);
use File::Basename;
use File::Path;
use File::Spec;

use FIND;
use CONFIG;
use DTR;
use TMGR;
use SYSTEM;
use LOG::Handler;
use ERROR::Handler;

sub new
{

# Argument List
# =============

  my $invocant = shift;
  my $path   = shift;
  my %config = scalar(@_) ? @_ : ();

  my $self = {};
  my $class = ref($invocant) || $invocant;

# Find the input transaction
# files.
# ==========================

  my $find = FIND->new($path);
  my @transaction = $find->files(".*\.dtr\$");

# Retrieve Data transactions
# ==========================
 
  foreach my $pathname (@transaction) {

    $pathname = abs_path($pathname);
    my $cfg   = CONFIG->new($pathname,%config);
    my $dtr   = DTR->new($pathname,%$cfg);

    my $inputType = DMGR::newKey($self,$dtr->{PDR_SHORT_NAME});
    $self->{$inputType} = $dtr;

  }

  bless($self,$class);

}

sub newKey { my $self = shift;
             my $key  = shift;

  my $newkey = $key;
  my $occurrence = 0;

  while (exists $self->{$newkey}) {
    $newkey = $key . "_" . $occurrence;
    $occurrence += 1;
  }

  return $newkey;

}

sub stat
{

  DMGR:    my $self = shift;
  STRING:  my $type = shift;
  HASH:    my %options = scalar(@_) ? @_ : ();

  DTR:     my $dtr = $self->{$type}; $dtr or return ();

  $options{MIN_FILE_COUNT} = $options{MIN_FILE_COUNT} // $dtr->{MIN_FILE_COUNT};
  $options{MIN_FILE_COUNT} = 1 if $options{CLOCK};

  STRING:  my @pdr  = $dtr->search(%options);
  TMGR:    my $tmgr = TMGR->new($dtr->{DTR_DIR_ID});
  SYSTEM:  my $sys  = SYSTEM->new($dtr->{DTR_DIR_ID});

  INTEGER: my $file_count = @pdr;
  INTEGER: my $min_file_count = $options{MIN_FILE_COUNT} || 1;

  BOOLEAN: my $expired = $tmgr->isExpired(%$dtr, %options);
  BOOLEAN: my $enabled = $sys->isEnabled($dtr->{DTR_DIR_ID});
  BOOLEAN: my $success = ($file_count >= $min_file_count) ? 1 : 0;

  $success = 0 if ! $enabled;
  $success = 0 if ! $dtr->{SEARCH_RESULT};

  BOOLEAN: my $waiting = $enabled ? ! ($success or $expired) : 0;

  $dtr->{SUCCESS}   = $success;
  $dtr->{EXPIRED}   = $expired;
  $dtr->{ENABLED}   = $enabled;
  $dtr->{PDR_COUNT} = $file_count;
  $dtr->{PDR_FILES} = \@pdr;
  $dtr->{WAITING}   = $waiting;

  return ($success, $expired, $enabled, $waiting, $file_count, @pdr);

}

sub mstat { REFERENT:  my $self = shift;
            ARRAY_REF: my $types   = scalar(@_) ? shift : [keys %$self];
            HASH:      my %options = scalar(@_) ? @_ : ();

  DTR:       my $dtr;
  ARRAY_REF: my $pdr;
  INTEGER:   my $file_count;
  STRING:    my ($type, @pdr);
  BOOLEAN:   my ($success, $expired, $enabled, $waiting) = (1,0,1,0);

  $types = ref($types) ? $types : [ $types ];

  foreach $type (@$types) {

    $self->{$type} or next;

    $self->stat($type,%options);

    $dtr = $self->{$type};

    $success  = $dtr->{SUCCESS} && $success;
    $expired  = $dtr->{EXPIRED} || $expired;
    $enabled  = $dtr->{ENABLED} && $enabled;
    $waiting  = $dtr->{WAITING} || $waiting;

    $file_count += $dtr->{PDR_COUNT};
    $pdr         = $dtr->{PDR_FILES};

    push @pdr, @$pdr;

  }

  return ($success, $expired, $enabled, $waiting, $file_count, @pdr);

}

sub execute { REFERENT: my $self = shift;
              STRING:   my $type = shift;

  DTR: my $dtr = $self->{$type};
  $dtr or return;

  my $sys = SYSTEM->new($dtr->{DTR_DIR_ID});
  $sys->isEnabled() or return;

  $dtr->isScheduled() or return;

  my ($tstart, $tend, $inc_sec) = $dtr->times();

  my $t = $tstart;

  while ($t <= $tend) {

    my $date = $t->strftime("%Y%m%d");
    my $time = $t->strftime("%H%M%S");

    my $now = CLOCK->new();
    my $system_time = $now->strftime("%Y%m%d%H%M%S");

    my $dmgr_local = DMGR->new($dtr->{DTR_DIR_ID}, %$dtr,
                             DATE=>$date,
                             TIME=>$time,
                             SYSTEM_TIME=>$system_time);

    my $dtr_local = $dmgr_local->{$type};

    $dtr_local->genProd();

#   my $dtr_local = DTR->new($dtr->{DTR_FILE_ID}, %$dtr, 
#                            DATE=>$date,
#                            TIME=>$time,
#                            SYSTEM_TIME=>$system_time);

    $dtr->{FILE_LIST} and do {
      $self->derivePDR($dtr_local);
      $t += $inc_sec;
      next;
    };

    $dtr->{PDR_REMOTE_DIR} and do {
      $self->importPDR($dtr_local);
      $t += $inc_sec;
      next;
    };

    my @pdr = $self->createPDR($dtr_local);

    $dtr->{PDR_EXPORT_DIR} and do {
      $self->exportPDR($dtr_local,@pdr);
      $t += $inc_sec;
      next;
    };

    $t += $inc_sec;

  }

}

sub mexecute { REFERENT:  my $self = shift;
               ARRAY_REF: my $types   = scalar(@_) ? shift : [keys %$self];
               HASH_REF:  my $options = scalar(@_) ? shift : {};

  FUNCTION_NAME: my $this = "DMGR::mexecute";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

# Retrieve all of the inputs
# ==========================

  BOOLEAN: my $waiting = 0;

  foreach my $type (@$types) {

    $self->execute($type);

    my $dtr      = $self->{$type};
    %$options = (%$options, %$dtr);

    my ($success, $expired, $enabled) = $self->stat($type);

    $enabled or next;
    next if $success;

    if ( $expired ) { $eh->warning(1,TYPE=>$type); next }

    $waiting = 1;
    $eh->advisory(2,TYPE=>$type);

  }

  return 0 if $waiting;

  return 1;

}

sub importPDR
{
  REFERENT: my $self = shift;
  DTR:      my $dtr  = shift;

  FUNCTION_NAME: my $this = "DMGR::importPDR";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  STRING:     my $disposition;
  STRING:     my ($pdr, @pdr);
  DTR_OBJECT: my @product;

  @pdr = $dtr->getPDR();

  foreach $pdr (@pdr) {

    if ($dtr->isaSuccess($pdr)) { next; }

    $disposition = $dtr->valPDR($pdr);

    $disposition =~ /SUCCESS/ or 
        $eh->warning(2,PDR=>$pdr,DISP=>$disposition);

    if ( $dtr->notify($pdr) ) {next; }

    my $dtr_local = $dtr->resolve($pdr);
    @product = $dtr_local->getProd($pdr);

    $disposition = $dtr->valProd($pdr,@product);

    if ($dtr->isaSuccess($pdr)) { print "$pdr is a success\n" }
    else { $eh->warning(1,PDR=>$pdr,DISP=>$disposition) }

    $dtr->moveProd(@product) if $dtr->isaSuccess($pdr);

    if ( $dtr->notify($pdr) ) {next; }
  }

}

sub exportPDR
{

  REFERENT: my $self = shift;
  DTR:      my $dtr  = shift;
  STRING:   my @pdr = scalar(@_) ? @_ : ();

  STRING:     my $pdr;
  STRING:     my $pathname;

  foreach $pdr (@pdr) {

    $dtr->isaSuccess($pdr) or next;
    $pathname = $dtr->exportPDR($pdr);
    print "$pdr was exported to $pathname\n";

  }

}

sub createPDR
{

  REFERENT: my $self = shift;
  DTR:      my $dtr  = shift;

  FUNCTION_NAME: my $this = "DMGR::createPDR";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  DTR_OBJECT: my @product;
  STRING:     my $disposition;
  STRING:     my $pdr;

  $pdr = $dtr->{PDR_PATHNAME};
  return $pdr if $dtr->isaSuccess($pdr);

  $pdr = $dtr->genPDR();

  @product = $dtr->getProd($pdr);

  $disposition = $dtr->valProd($pdr,@product);

  if ($dtr->isaSuccess($pdr)) { print "$pdr is a success\n" }
  else { $eh->warning(1,PDR=>$pdr,DISP=>$disposition) }

  $dtr->moveProd(@product) if $dtr->isaSuccess($pdr);

  return $pdr;

}

#******************************************************************************
sub derivePDR { REFERENT: my $self = shift;
                DTR:      my $dtr  = shift;
#******************************************************************************
# English Name: Derive Product Delivery Record
# -------------
#
# Purpose: Creates product delivery records using information described in the
# -------- specified data transaction (DTR). This type of transaction uses
#          file listings to discover and derive PDRs. This is necessary when
#          the names of files are indeterminate.
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
#           04/28/2014      J.Ardizzone  documented.
#******************************************************************************

  FUNCTION_NAME: my $this = "DMGR::derivePDR";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  CLOCK:      my $t = CLOCK->new();
  STRING:     my ($pdr, @pdr);
  STRING:     my $method = $dtr->{CLOCK_METHOD} // "timeless";

# Retrieve a listing of files that 
# will be used to derive the PDRs.
# ================================

  my $file_list = $dtr->{FILE_LIST};
  my $glob = $file_list // File::Spec->catfile($dtr->{DIRECTORY_ID}, "*");
  my @files = < $glob >;

# Create and execute the PDRs.
# ============================

  foreach my $pathname (@files) {

    my $name = basename $pathname;

    my @nodes        = split /\./, $name;
    my $name_no_type = join '.', (@nodes)[0..$#nodes-1];

#   Use the configured clock method
#   to extract the date/time from
#   the file.
#   -------------------------------

    $t       = $t->$method($pathname,DATETIME=>1) // CLOCK->new();
    my $date = $t->strftime("%Y%m%d");
    my $time = $t->strftime("%H%M%S");

#   Create a transaction using
#   the newly derived parameters.
#   -----------------------------

    my $dtr_local = DTR->new($dtr->{DTR_FILE_ID},%$dtr,
                             DATE=>$date,
                             TIME=>$time,
                             FILE_NAME=>$name,
                             FILE_NAME_NO_TYPE=>$name_no_type);

#   Save the PDR filename
#   ---------------------

    $pdr = $dtr_local->{PDR_PATHNAME};
    push @pdr, $pdr;

#   Skip to the next PDR if the
#   current one has already been
#   executed successfully.
#   ----------------------------

    next if $dtr_local->isaSuccess($pdr);

#   Generate the PDR and validate
#   the data transaction.
#   -----------------------------

    $pdr = $dtr_local->genPDR();

    my @product = $dtr_local->getProd($pdr);

    my $disposition = $dtr_local->valProd($pdr,@product);

    if ($dtr->isaSuccess($pdr)) { print "$pdr is a success\n" }
    else { $eh->warning(1,PDR=>$pdr,DISP=>$disposition) }

    $dtr_local->moveProd(@product) if $dtr_local->isaSuccess($pdr);

  }

  return @pdr;

}

sub mfiles
{

  DMGR:    my $self = shift;
  STRING:  my $types = shift;
  HASH:    my %options = scalar(@_) ? @_ : ();

  STRING:  my ($type, @files);

  $types = ref($types) ? $types : [ $types ];

  foreach $type (@$types) {
    $self->{$type} or next;
    push @files, ($self->files($type,%options));
  }

  return sort @files;

}

sub files
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING:   my $type = shift;
  HASH:     my %options = scalar(@_) ? @_ : ();

# Local Files
# -----------

  DTR:    my $dtr;
  ANY:    my @result;
  STRING: my ($pdr, @pdr, @files);

# Locate the PDRs.
# ================

  @result = $self->stat($type,%options);
  @pdr = (@result)[5..$result[4]+4];

  $dtr = $self->{$type};
  foreach $pdr (@pdr) { push @files, ($dtr->files($pdr,\%options)) };

  return sort @files;
}

sub error_handler { my $error_handle = shift;
                    my $error_code = shift;
                    my %options = scalar(@_) ? @_ : ();

  my $type = $options{ERROR_TYPE};
  my $lh   = LOG::Handler->new(HANDLE=>$error_handle);

# Do not print elapsed time for 
# this module.
# =============================

  defined $options{ERROR_ELAPSED_TIME} and return;

# Trap traceback event
# ====================

  $options{ERROR_TRACEBACK} and do {

    $lh->error($error_code,"Traceback: $error_handle() encountered an error.");
    return;

  };

# Comments have a specified
# return code of 0.
# =========================

  $error_code or return $lh->comment(0,$options{COMMENT});

# mexecute() error handle
# =======================

  $error_handle eq "DMGR::mexecute" and do {

    $error_code == 1 and do {
      $lh->$type("DTR-001","Time has expired for type: \"$options{TYPE}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type("DTR-002","Waiting for data type: \"$options{TYPE}\"");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

  };

# createPDR() error handle
# ========================

  $error_handle eq "DMGR::createPDR" and do {

    $error_code == 1 and do {
      $lh->$type("DTR-003","Transaction failed for: " .
                                "\"$options{PDR}\": $options{DISP}");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

  };

# importPDR() error handle
# ========================

  $error_handle eq "DMGR::importPDR" and do {

    $error_code == 1 and do {
      $lh->$type("DTR-003","Transaction failed for: " .
                                "\"$options{PDR}\": $options{DISP}");
      return;
    };

    $error_code == 2 and do {
      $lh->$type("DTR-004","Transaction failed for: " .
                                "\"$options{PDR}\": $options{DISP}");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

# derivePDR() error handle
# ========================

  $error_handle eq "DMGR::derivePDR" and do {

    $error_code == 1 and do {
      $lh->$type("DTR-003","Transaction failed for: " .
                                "\"$options{PDR}\": $options{DISP}");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

  print STDERR "No error handle for \"$error_handle\"\n";

  return;

}

1;
