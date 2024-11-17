package CONFIG;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use CLOCK;

#******************************************************************************
sub new
#******************************************************************************
# English Name: Package Constructor
# -------------
#
# Purpose: Instantiates a CONFIG object. A hash table referent is returned
# -------- as a dictionary of resolved parameter definitions. An optional
#          directory path supplied to this constructor will searched from
#          parent to child for "config" files containing parameter definitions.
#          See the module prolog for more information.
#
# Language: Perl
# ---------
#
# Usage: $cfg = CONFIG->new($path)
# ------
#
# Notes:
# ------
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $cfg                referent      OUT  hash table of resolved configuration
#                                        parameters.
#
# $path                 string       IN  directory path containing configuration
#                                        files. Configuration files are resolved
#                                        starting with the first parent
#                                        directory containing the ".root"
#                                        empty file. See module prolog for
#                                        more information.
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           06/30/2013      J.Ardizzone  created.
#******************************************************************************
{
  my $invocant = shift;
  my $class = ref($invocant) || $invocant;
  my $path = scalar(@_) ? shift : undef;
  my $self = scalar(@_) ? { @_ } : {};

  my $n;
  my @dirs;
# my $self = {};
  my ($root, $file);

  if ( ! defined($path)) { return bless($self, $class); }

# Retrieve a list of 
# directories from the path.
# ==========================

  @dirs = File::Spec->splitdir( $path );

# Traverse the path from child to ancestor
# to locate the first occurrence of the
# .root empty file. The directory hosting
# this file is the root configuration
# directory.
# ========================================

  $n = $#dirs;
  foreach (reverse (0..$#dirs)) {

    $n = $_;
    $root = File::Spec->catdir(@dirs[0..$n], ".root");
    if ( -f "$root" ) { last; }

  }

# Starting with the root configuration
# directory, extract configuration
# parameters from ancestor to child,
# overriding ancestor settings as specified.
# ==========================================

  foreach ($n..$#dirs) {

    $file = File::Spec->catdir(@dirs[0..$_], "config");
    if ( -f "$file") { CONFIG::configure($file,$self) }

  }

  bless($self, $class);

}

#******************************************************************************
sub configure()
#******************************************************************************
# English Name: Configure
# -------------
#
# Purpose: Resolves configuration parameter files (see module prolog) using an
# -------- internally maintained dictionary (self referent) and an optionally
#          supplied hash of parameter definitions that override the internal
#          dictionary.
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
#           07/01/2013      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  my $self = ref($_[0]) ? shift : {};
  my $file = shift;
  my $config = scalar(@_) ? shift : $self;

# Local Variables
# ---------------

  my $entry = 0;
  my @array = ();
  my ($key, $value);
  my $cfg   = CONFIG->copy(%$self,%$config);
  my $clock = CLOCK->new(%$self, %$config);
  my $dictionary = $config->{DICTIONARY} // {};

  open PARAMETERS, "<$file";

  while (<PARAMETERS>) {

    chomp;

    if ( /(^\s*)(\w+)(\s*)(=)(\s*)(.*)/ ) {

      $key = "$2";
      $value = "$6";
      $value =~ s/;$//;

      $value = $cfg->var_interp($value);
      $value = $cfg->time_interp($value,$clock) if ! $cfg->{TIMELESS};

      $value = $dictionary->{$key} if exists $dictionary->{$key};

      $cfg->{$key}    = $value;
      $config->{$key} = $value;
      $array[$entry++] = "$key = $value";

    }

  }

  close PARAMETERS;

  return @array;
}

sub jobConfig
{

# Argument List
# -------------

  REFERENT: my $self   = shift;
  STRING:   my $src    = shift;
  STRING:   my $dest   = shift;
  HASH:     my %config = scalar(@_) ? @_ : ();

# Local Variables
# ===============

  CONFIG:      my $cfg   = CONFIG->copy(%$self,%config);
  FILE_HANDLE: my ($fh_in, $fh_out);
  STRING:      my ($file, $template, @template, $glob);

  mkpath $dest or return 0 if ! -d $dest;

# Configure runtime files
# ========================

  $glob = -d $src ? File::Spec->catfile($src,"*") : $src;

  @template = < $glob >;

  foreach $template (@template) {

    $file = basename $template;
    $file = File::Spec->catfile($dest,$file);

    open $fh_in, "<$template"; open $fh_out, ">$file";
    $cfg->sed($fh_in,$fh_out);
    close $fh_in; close $fh_out;
  }

  return 1;

}

#******************************************************************************
sub sed
#******************************************************************************
# English Name: Stream Editor
# -------------
#
# Purpose: Resolves file templates containing parameter variables (see module
# -------- prolog) using an internally maintained dictionary (self referent)
#          and an optionally supplied hash of parameter definitions that
#          override the internal dictionary.
#
# Language: Bourne Shell
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
#           07/01/2013      J.Ardizzone  created.
#******************************************************************************
{

# Argument List
# -------------

  my $self = ref($_[0]) ? shift : {};
  my $reader = shift;
  my $writer = shift;
  my %options = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  my $name;
  my $entry = 0;
  my ($text, @file);
  my $cfg   = CONFIG->copy(%$self,%options);
  my $clock = CLOCK->new(%$self,%options);
  my $clock2 = CLOCK->new(%$self,%options,DATE=>$cfg->{DATE2},TIME=>$cfg->{TIME2});

  while (<$reader>) {

    chomp;
    my $input = "$_";

    foreach my $value ($cfg->tuples($input,\$name)) {

      my $config = $name ? { $name => $value } : {};

      $text = $cfg->var_interp($input,$config);
      $text = $cfg->time_interp($text,$clock) if ! $cfg->{TIMELESS};
      $text = $cfg->time_interp($text,$clock2) if ! $cfg->{TIMELESS};

      print $writer "$text\n";
      $file[$entry++] = $text;

    }

  }

  return @file;

}

sub tuples { my $self = shift;
             my $string = shift;
             my $name = shift;

  my $value;
  my @value = ($string);
  my $ld    = $self->{DELIMITER} // '<';
  my $rd    = $self->{DELIMITER} // '>';

  $$name = undef;
  $string =~ /$ld(\w+)$rd/ or return @value;

  $$name = $1;
# $value = $self->{$$name} // $$name;
# $value = exists $self->{$$name} ? $self->{$$name} : $$name;
  $value = exists $self->{$$name} ? $self->{$$name} : undef;
  @value = ref($value) ? @$value : ($value);

  return @value;

}

sub var_interp
{

# Argument List
# -------------

  my $self = ref($_[0]) ? shift : {};
  my $string = shift;
  my $config = scalar(@_) ? shift : {};

  my $dictionary = $self->{DICTIONARY} // {};
  my $ld         = $self->{DELIMITER} // '<';
  my $rd         = $self->{DELIMITER} // '>';

  my ($var_name, $value);

  while ( $string =~ /$ld(\w+)$rd/ ) {

    $var_name = $1;

    $value = undef;
    if (exists $self->{$var_name}) { $value = $self->{$var_name}; }
    if (exists $config->{$var_name}) { $value = $config->{$var_name}; }

    $value = $dictionary->{$var_name} if exists $dictionary->{$var_name};

    $string =~ s/$ld\w+$rd/$value/;

  }

  return $string;

}

sub time_interp
{

# Argument List
# -------------

  my $self = ref($_[0]) ? shift : {};
  my $string = shift;
  my $clock = scalar(@_) ? shift : CLOCK->new(%$self);

  $string = $clock->strftime($string);

  return $string;

}

sub add
{

  my $self = shift;
  my %config = scalar(@_) ? @_ : ();
  
  foreach my $key (keys %config) { $self->{$key} = $config{$key}; }

}

sub copy
{
  my $invocant = shift;
  my $class = ref($invocant) || $invocant;

  my %self = scalar(@_) ? @_ : ();

  bless (\%self,$class);
}

sub clone
{
  my $invocant = shift;
  my $class = ref($invocant) || $invocant;

  my %self = (%$invocant, @_);

  bless (\%self,$class);
}

sub keys
{
  my $self = shift;
  my $file = shift;

  -f $file or return ();

  my @keys = ();
  my $config = CONFIG->new();
  my @file = $config->configure($file);

  foreach my $line (@file) { push @keys, (split /\s*=\s*/, $line)[0] }

  return @keys;
}

sub hash
{
  my $self = shift;
  my @keys = scalar(@_) ? @_ : ();

  @keys or return;

  my %config = ();
  @config{@keys} = @{$self}{@keys};

  return %config;
}

1;
