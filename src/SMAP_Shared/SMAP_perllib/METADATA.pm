package METADATA;

########################## API ################################
# $meta   = METADATA->new(%config,LOG_HANDLE=>xxxxxx)
# %config = $meta->execute(%config)
# $crid   = $meta->compare(%crid_config,%last_config)
# %config = $meta->read(fname,$group)
###############################################################

  use strict;

  use File::Basename;
  use File::Path;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long qw(GetOptionsFromString);

  use LOG::Handler;
  use ERROR::Handler;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Package Constructor
# -------------
#
# Purpose: Instantiates a metadata object. A hash table referent is returned
# -------- as a dictionary of initial parameter definitions.
#
# Language: Perl
# ---------
#
# Usage: $meta = METADATA->new(%config,%options)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $meta               referent      OUT  hash table of parameter definitions.
#
# %config                 hash   OPT,IN  optional hash table of parameter
#                                        definitions. Parameters specified
#                                        here are typically static and used
#                                        to resolve other parameters. They are
#                                        overriden if repeated in subsequent
#                                        method invocations with parameter
#                                        inputs.
#
# %options                hash   OPT,IN  hash table inputs that are intercepted
#                                        and interpreted as optional arguments.
#                                        They are not retained in the returned
#                                        referent of hash table parameters.
#
#   LOG_HANDLE             key   OPT,IN  specified the log handle to be used
#                                        as the I/O filter for all errors and
#                                        messages issued by the methods of this
#                                        module.
#                           
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/02/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  INVOCANT: my $invocant = shift;
  REFERENT: my $self = scalar(@_) ? { @_ } : {};

  my $class = ref($invocant) || $invocant;

  return bless($self, $class);

}

#******************************************************************************
sub execute
#******************************************************************************
# English Name: Execute
# -------------
#
# Purpose: Executes methods to resolve dynamic metadata parameters specified
# -------- as key/command pairings in the input hash table.
#
# Language: Perl
# ---------
#
# Notes: 1. This method currently recognizes the following commands: (1) h5dump 
# ------    (2) uname, (3) module. Parameter values that do not begin with
#           and of these names are copied unchanged into the returned hash.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# %config                 HASH       IN  hash table of parameter name/command
#                                        pairings (see note-1).
#
# %meta                   HASH      OUT  function return value: a copy of the
#                                        input hash table with commands resolved
#                                        into values.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/04/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  HASH:     my %config = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  FUNCTION_NAME: my $this = "METADATA::execute";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  HASH: my %meta = ();
  STRING: my ($key, $command);

# Resolve dynamic metadata
# ========================

  foreach $key (keys %config) {

    $command = $config{$key};

    foreach ($command) {

      /^h5dump -/ and do { $config{$key} = $self->h5dump($command); next };
      /^ncdump -/ and do { $config{$key} = $self->ncdump($command); next };
      /^uname -/  and do { $config{$key} = $self->uname($command); next };
      /^module -/ and do { $config{$key} = $self->module($command); next };

      /(^\w+) -/    and do { $eh->error(1,arg=>$1); return () };

    }

    $meta{$key} = $config{$key};
  }

  return %meta;

}
#******************************************************************************
sub h5dump
#******************************************************************************
# English Name: HDF-5 Dump
# -------------
#
# Purpose: Wrapper method for invoking the h5dump utility to extract attribute
# -------- and value information. Only the "-a" and "-g" options are implemented
#          in this method.
#
# Language: Perl
# ---------
#
# Usage: $value = $self->h5dump("-a <attribute_name> <filename>");
# ------ %meta  = $self->h5dump("-g <group_name> <filename>");
#        $version = $self->h5dump("-V");
#
# Notes: 1. See the h5dump manual for a description of the command-line usage.
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $command              STRING       IN  command-line using the same syntax
#                                        used by h5dump for the "-a" and "-g"
#                                        option.
#
# $value                   ANY      OUT  An attribute value is returned when
#                                        the "-a" option is specified.
#
# %meta                   HASH      OUT  A hash table of attribute/value pairs
#                                        is returned when the "-g" option is
#                                        specified.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/04/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING: my ($command, $default) = split /\s*[|]\s*/, shift;

# Local Variables
# ---------------

  FUNCTION_NAME: my $this = "METADATA::h5dump";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  INTEGER:     my $rc;
  FILE_HANDLE: my $fh;
  ARRAY_REF:   my $args;
  ANY:         my $value;
  HASH:        my %meta = ();
  BOOLEAN:     my $version = 0;
  INTEGER:     my $block_count = 0;
  STRING:      my ($attribute, $group, $dataset, $fname, $name);


# Parse the command-line arguments
# ================================

  ($rc, $args) = GetOptionsFromString($command,"a=s" => \$attribute,
                                               "g=s" => \$group,
                                               "d=s" => \$dataset,
                                               "V"   => \$version);

# Pop off the command name if included
# in the string.

  shift @$args if $args->[0] eq "h5dump";

  $fname = $args->[0];

# Return scalar attribute value
# when "-a" option is used.
# =============================

  if ($attribute) {

    $value = $default;
    -f $fname or return $value;

    open $fh, "h5dump -a $attribute $fname 2> /dev/null |";

    while (<$fh>) {
      chomp;
      /(\(0\):) (.*)/ and do { $value = $2; $value =~ s/"//g; next }
    }

    close $fh;

    return $value;

  }

# Return hash table of attribute
# names and values when "-g" or
# "-d" option is used.
# ===============================

  my $id  = $group ? "GROUP" : "DATASET";
  my @opt = $group ? ("-g", $group) : ("-d", $dataset);

  if ($group or $dataset) {

    -f $fname or return ();

    open $fh, "h5dump -A @opt $fname 2> /dev/null |" 
             or die "cannot pipe from h5dump: $!";

    while (<$fh>) {

      chomp;

      ++$block_count if /\{/;
      --$block_count if /\}$/;

      /($id) (.*) \{/ and do { last if $name; next };

      /(ATTRIBUTE) (.*) \{/ and do { $name = $2; $name =~ s/"//g; next };

#     $name or next;
      next if $block_count != 4;

      /(\(0\):) (.*)/ and do { $value = $2; $value =~ s/"//g;
                               $meta{$name} = $value; next };

#     $name = undef;

    }

    close $fh;

    return %meta;

  }

# Return hdf-5 version number
# ===========================

  if ($version) {

   open $fh, "h5dump -V |" or die "cannot pipe from h5dump: $!";

   $version = (split /:\s/, <$fh>)[1];
   close $fh;

   chomp $version;
   return $version;

  }

  return 0;

}  

#******************************************************************************
sub ncdump
#******************************************************************************
# English Name: NetCDF Dump
# -------------
#
# Purpose: Wrapper method for invoking the ncdump utility to extract attribute
# -------- and value information. Dataset specific options have been implemeted
#          in this method due to the unique conventions employed by various
#          data providers for storing metadata. 
#          
# Language: Perl
# ---------
#
# Usage: $value = $self->ncdump("-t $type -a $attribute_name $filename");
# ------ %meta  = $self->ncdump("-t $type -g $group_name $filename");
#
# Notes: 1. See the ncdump manual for a description of the command-line usage.
# ------    The options implemented in this method use the native options in
#           combination with other unix utilities.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $type                 string       IN  designator for the dataset type. This
#                                        option is used to parse the returned
#                                        metadata from "ncdump -h". Current
#                                        dataset types are:
#
#                                        fpar: MODIS FPAR MOD15A2
#
# $attribute_name       string       IN  name of attribute to be queried.
#
# $filename             string       IN  name of netCDF readable file.
#
# $value                   ANY      OUT  An attribute value.
#
# %meta                   HASH      OUT  A hash table of attribute/value pairs.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           09/01/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING: my ($command, $default) = split /\s*[|]\s*/, shift;

# Local Variables
# ---------------

  FUNCTION_NAME: my $this = "METADATA::ncdump";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  my %meta = ();
  my $value = $default;
  my ($attribute, $attr_name);
  my ($type, $group, $group_name);

# Parse the command-line arguments
# ================================

  my ($rc, $args) = GetOptionsFromString($command,"a=s" => \$attribute,
                                                  "g=s" => \$group,
                                                  "t=s" => \$type);

# Pop off the command name if included
# in the string.

  shift @$args if $args->[0] eq "ncdump";

# Return if file does
# not exist.
# ===================

  my $fname = $args->[0];
  -f $fname or return $value;

# FPAR Metadata Parsing
# =====================

  $type eq "fpar" and do {

    open my $fh, "ncdump -h $fname 2> /dev/null |"
              or die "cannot pipe from ncdump: $!";

    while (my $line = <$fh>) {

      chomp $line;

      $line =~ s/\\\"/\"/g;
      $line =~ s/\\n/@/g;
      $line =~ s/@/\n/g;

      my @lines = split /\n/, $line;

      foreach (@lines) {

        /^\s*GROUP\s+=\s+(.*)/ and do { $group_name = $1; next };

        /^\s*OBJECT\s+=\s+(.*)/ and do { $attr_name = $1; next };

        /^\s*VALUE\s+=\s+(.*)/ and do { 

          $value = $1; $value =~ s/"//g;

          $attr_name eq $attribute and return $value;

          $group_name eq $group or next;

          $meta{$attr_name} = $value;

        };

      }

    };

    close $fh;

  };

  $group and return %meta;

  return $default;

}  
#******************************************************************************
sub uname
#******************************************************************************
# English Name: Unix uname - print system information
# -------------
#
# Purpose: Wrapper method for invoking the unix "uname" command for querying 
# -------- system information.
#
# Language: Perl
# ---------
#
# Notes: 1. See the "uname" manual for a description of the command-line usage.
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $command              STRING       IN  command-line using the same syntax
#                                        used by the unix "uname" command.
#
# $value                   ANY      OUT  function return value: returned string
#                                        from the Unix "uname" command using the
#                                        option specified in the command-line
#                                        string.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/04/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING: my $command = shift;

# Local Variables
# ---------------

  INTEGER:     my $rc;
  ARRAY_REF:   my $args;
  STRING:      my $value;
  BOOLEAN:     my ($a,$s,$n,$r,$v,$m,$p,$i,$o);

# Parse the command-line arguments
# ================================

  ($rc, $args) = GetOptionsFromString($command,"a" => \$a,
                                               "s" => \$s,
                                               "n" => \$n,
                                               "r" => \$r,
                                               "v" => \$v,
                                               "m" => \$m,
                                               "p" => \$p,
                                               "i" => \$i,
                                               "o" => \$o);

  $rc or die "Error parsing options for \"uname\" command";

# Return the result
# =================

  chomp($value = `uname -a`) if $a;
  chomp($value = `uname -s`) if $s;
  chomp($value = `uname -n`) if $n;
  chomp($value = `uname -r`) if $r;
  chomp($value = `uname -v`) if $v;
  chomp($value = `uname -m`) if $m;
  chomp($value = `uname -p`) if $p;
  chomp($value = `uname -i`) if $i;
  chomp($value = `uname -o`) if $o;

  return $value;
}

#******************************************************************************
sub module
#******************************************************************************
# English Name: Modules Package Command
# -------------
#
# Purpose: Wrapper method for invoking the Modules Package command for querying 
# -------- system information.
#
# Language: Perl
# ---------
#
# Notes: 1. This wrapper uses a modified syntax to isolate the requested
# ------    module.
#
#        2. The "module" command is not invoked. This method parses the
#           LOADEDMODULES environment variable.
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $command              STRING       IN  command-line using the following
#                                        syntax:
#
#                                        module -list comp
#                                        module -list lib
#                                        module -list mpi
#
# $value                   ANY      OUT  function return value: returned string
#                                        containing the value of the specified
#                                        module component.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           02/04/2014      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING: my $command = shift;

# Local Variables
# ---------------

  INTEGER:     my $rc;
  ARRAY_REF:   my $args;
  STRING:      my ($module, @modules);
  STRING:      my ($type, $key, $value);

# Parse the command-line arguments
# ================================

  ($rc, $args) = GetOptionsFromString($command,"list=s" => \$type);

  $rc or die "Error parsing options for \"module\" command";

# Return the result
# =================

  @modules = split /:/, $ENV{LOADEDMODULES};

  foreach $module (@modules) {

    ($key, $value) = split /\//, $module;
    return $value if $type eq $key;

  }

  return 0;
}

sub read
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING: my $filename = shift;
  STRING: my $group = scalar(@_) ? shift : undef;

# Query the attributes for the default
# metadata group unless a group is specified;
# ===========================================

  return $self->h5dump("-g " . $group . " " . $filename) if $group;
  return $self->h5dump("-g " . "Metadata/Config " . $filename);
}

#******************************************************************************
sub compare { REFERENT: my $self = shift;
              HASH_REF: my $old =  shift;
              HASH_REF: my $new =  shift;
#******************************************************************************
# English Name: Compare
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

  FUNCTION_NAME: my $this = "METADATA::compare";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  BOOLEAN:  my $changed = 0;
  HASH_REF: my $changeLog = {};

# Check for deleted or
# changed parameters.
# ====================

  foreach my $key (keys %$old) {

    exists $new->{$key} or do { 

      $eh->advisory(1,NAME=>$key);
      $changeLog->{$key} = "Parameter was deleted.";
      next
    };

    $new->{$key} or next;
    $old->{$key} or next;

    $new->{$key} =~ s/^\s+|\s+$//g;
    $old->{$key} =~ s/^\s+|\s+$//g;

    $new->{$key} eq $old->{$key} or do {

      $eh->advisory(2,NAME=>$key,OLD=>$old->{$key},NEW=>$new->{$key});
      $changeLog->{$key} = "Value changed from \"$old->{$key}\" to " .
                           "\"$new->{$key}\".";
      $changed = 1;
      next
    };

  }

# Check for added parameters
# ==========================

  foreach my $key (keys %$new) {

    exists $old->{$key} or do {

      $eh->advisory(3,NAME=>$key,NEW=>$new->{$key});
      $changeLog->{$key} = "Parameter was added. Value is \"$new->{$key}\".";
      next
    };

  }

  $self->{changeLog} = $changeLog;

  return ! $changed;

}

#******************************************************************************
sub update { REFERENT: my $self = shift;
             HASH_REF: my $old =  shift;
             HASH_REF: my $new =  shift;
#******************************************************************************
# English Name: Update
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
#           02/04/2014      J.Ardizzone  created.
#           06/03/2014      J.Ardizzone  changed CRID from 3 to 4 digits.
#******************************************************************************

  FUNCTION_NAME: my $this = "METADATA::update";
  ERROR_HANDLER: my $eh = ERROR::Handler->new(\&error_handler,$this);

  BOOLEAN:  my $isaMatch  = $self->compare($old,$new);
  HASH_REF: my $changeLog = $self->{changeLog} // {};

  $isaMatch and return %$changeLog;

  CRID:    my $old_crid = $new->{CompositeReleaseID};
  INTEGER: my $crid = substr($old_crid,6,4);

  $crid = sprintf("%04d",$crid + 1);
  $crid = "001" if $changeLog->{VersionID};

  $new->{CompositeReleaseID} = $new->{VersionID} . $crid;

  CRID: my $new_crid = $new->{CompositeReleaseID};
  $eh->advisory(1,OLD=>$old_crid,NEW=>$new_crid);

  $changeLog->{CompositeReleaseID} = "Value changed from \"$old_crid\" " .
                                     "to \"$new_crid\".";

  return %$changeLog;

}

#******************************************************************************
sub addGroup { REFERENT: my $self = shift;
               STRING:   my $group = shift;
               HASH:     my %meta = scalar(@_) ? @_ : ();
#******************************************************************************
# English Name: Add Group
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

  ARRAY_REF: my $metadata = $self->{METADATA} // [];

  foreach my $key (keys %meta) { 

    STRING: my $attribute = $group . "/" . $key;
    STRING: my $value     = $meta{$key};

    $self->{$attribute} = $value;
    push @$metadata, $attribute;

  }

  $self->{METADATA} = $metadata;

}

sub write
{
  my $self = shift;
  my $fname = shift;

  -f $fname or return;

  my $fh;
  my $namelist = $fname . ".nml";
  my $metadata = $self->{METADATA} // [];

  @$metadata or return;

  open $fh, ">$namelist";

  foreach my $key (@$metadata) {

    my $name = $key;
    my $value = $self->{$key};

    print $fh " \&meta\n";
    print $fh "   name = \'$name\'\n";
    print $fh "   value = \'$value\' \/\n";
    print $fh "";

  }

  close $fh;

  system "insertMetaGroups.x $namelist $fname" and die "metadata insert failed";
  unlink $namelist;

  $self->{METADATA} = [];
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

# execute() error handle
# ======================

  $error_handle eq "METADATA::execute" and do {

    $error_code == 1 and do {
      $lh->$type("MTD-001","Query function is not recognized: \"" .
                                              $options{arg} . "\"");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

# h5dump() error handle
# =====================

  $error_handle eq "METADATA::h5dump" and do {

    $error_code == 1 and do {
      $lh->$type("MTD-010","Unable to query the attribute, \"" .
                            $options{arg} . "\" from file \"" . 
                            $options{arg1} . "\"");
      return;
    };

    $error_code == 1 and do {
      $lh->$type("MTD-011","Unable to query the group, \"" .
                            $options{arg} . "\" from file \"" . 
                            $options{arg1} . "\"");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

# compare() error handle
# ======================

  $error_handle eq "METADATA::compare" and do {

    
    $error_code == 1 and do {
      $lh->$type("MTD-001","The following parameter was " .
                              "deleted: \"$options{NAME}\"");
      return;
    };

    $error_code == 2 and do {
      $lh->$type("MTD-002","The value of \"$options{NAME}\" " .
                              "was changed from \"$options{OLD}\" " .
                              "to \"$options{NEW}\".");
      return;
    };

    $error_code == 3 and do {
      $lh->$type("MTD-003","The following parameter was " .
                              "added: NAME=\"$options{NAME}\", " .
                              "VALUE=\"$options{NEW}\".");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

# update() error handle
# =====================

  $error_handle eq "METADATA::update" and do {

    $error_code == 1 and do {
      $lh->$type("MTD-004","The Composite Release ID " .
                              "was changed from \"$options{OLD}\" " .
                              "to \"$options{NEW}\".");
      return;
    };

    print STDERR "$error_handle: Unknown error code: $error_code\n";

    return;

  };

  print STDERR "METADATA::error_handler: no error handle for \"$error_handle\"\n";

}

1;
