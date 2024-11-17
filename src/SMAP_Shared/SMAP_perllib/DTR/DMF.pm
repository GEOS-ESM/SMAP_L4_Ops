                        #######################
                        #                     #
                        #   PACKAGE DTR::DMF  #
                        #                     #
                        #######################

package DTR::DMF;
our @ISA = "DTR::TRANSACTION";

use strict;

use DTR::TRANSACTION;
use DTR::NULL;
use ERROR::Handler;
use File::Basename;
use File::Copy;
use File::Spec;

my $HANDLE;
my $DMF_DIR;

sub list
{

  my $self = shift;
  my $pathname = shift;

  my $name = basename $pathname;
  my $dir  = -d $pathname ? $pathname : dirname $pathname;

  $HANDLE = $self->handle($dir);

  my @keys = sort keys %$HANDLE;
  @keys    = -d $pathname ? @keys : grep /$name/, @keys;

  my @files = ();

  foreach my $name (@keys) {

    $HANDLE->{$name} or next;
    my $file = File::Spec->catfile($DMF_DIR, $name);
    push @files, $file;

  }

  return @files;

}

sub get
{
  my $self  = shift;
  my $dir   = shift;
  my @files = sort @_;

  my $this = "DTR::DMF::get";
  my $eh = ERROR::Handler->new(\*STDERR,$this);

  foreach my $file (@files) {

    my $path  = dirname $file;
    $HANDLE   = $self->handle($path);

    $HANDLE->getFile($dir, $file) or $eh->error(1,errstr=>"$file failed");

  }

  $eh->isError() and do { $eh->throw; return 0 };

  return 1;

}

sub put
{
  my $self = shift;
  my $dir  = shift;
  my @file = sort @_;

  my $this = "DTR::DMF::put";
  my $eh = ERROR::Handler->new(\*STDERR,$this);

  $HANDLE = $self->handle($dir);

  foreach my $in_file (@file) {

    -s $in_file or next;

    my $name     = basename $in_file;
    my $out_file = File::Spec->catfile($dir, $name);

    $HANDLE->{$name} and do {

      my ($in_mtime)  = (stat($in_file))[9];
      my ($out_mtime) = (stat($out_file))[9];

      next if $out_mtime >= $in_mtime;

    };

    $HANDLE->putFile($in_file) or $eh->error(1,errstr=>"$in_file failed");

  }

  $eh->isError() and do { $eh->throw; return 0 };

  return 1;

}

sub pack { my $self = shift;
           my $dir  = shift;

  my $this = "DTR::DMF::pack";
  my $eh = ERROR::Handler->new(\*STDERR,$this);

  $HANDLE = $self->handle($dir);

  $HANDLE->pack() or return $eh->error(1,errstr=>"pack failed for: $dir");

  return 1;
}

sub checksum { my $self     = shift;
               my $pathname = shift;

  my $handle = DTR::NULL->new();

  my $name = basename $pathname;
  my $path = dirname $pathname;
  my $registry = File::Spec->catfile($path, "dtr.list");

  -s $registry or return $handle->checksum($pathname);

  $HANDLE   = $self->handle($path);
  my $stats = $HANDLE->{$name} // [0,0,0];

  return $stats->[1];
}

sub filesize { my $self     = shift;
               my $pathname = shift;

  my $handle = DTR::NULL->new();

  my $name = basename $pathname;
  my $path = dirname $pathname;
  my $registry = File::Spec->catfile($path, "dtr.list");

  -s $registry or return $handle->filesize($pathname);

  $HANDLE   = $self->handle($path);
  my $stats = $HANDLE->{$name} // [0,0,0];

  return $stats->[2];
}

sub handle { my $self = shift;
             my $path = shift;

  if ($path ne $DMF_DIR) {

    $HANDLE = $HANDLE->close() if $HANDLE;
    $HANDLE = DTR::DMF::Handler->new($path,$self->{DTR_ARCHIVE_COPY});

  }

  $DMF_DIR = $path;

  return $HANDLE;
}

1;
