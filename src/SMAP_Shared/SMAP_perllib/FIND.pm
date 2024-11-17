package FIND;

use strict;

use File::Basename;
use File::Path;
use File::Spec;
use File::Find;

require Exporter;
use vars qw(@ISA @EXPORT $VERSION);
@ISA     = qw(Exporter);
@EXPORT  = qw(new);
$VERSION = 1.0;

my $self;

sub new
{
  my $invocant = shift;
  my $class    = ref($invocant) || $invocant;
  my $path     = scalar(@_) ? shift : ".";

  $self = [];
  find( {follow => 1, no_chdir => 1, wanted => \&FIND::wanted}, $path);

  bless ($self, $class);

}

sub wanted
{
  push @$self, $_;
}

sub files
{
  my $list = shift;
  my $pattern = scalar(@_) ? shift : ".*";

  my @files;
  foreach (@$list) { 
    (/$pattern/ && -f) && push @files, $_;
  }

  return @files;
}

1;
