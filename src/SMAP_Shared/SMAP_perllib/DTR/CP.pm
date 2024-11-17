                        #######################
                        #                     #
                        #   PACKAGE DTR::CP   #
                        #                     #
                        #######################

package DTR::CP;
our @ISA = "DTR::TRANSACTION";

use strict;

use DTR::TRANSACTION;
use File::Basename;
use File::Path;
use File::Spec;
use File::Copy;

use ERROR::Handler;

sub list
{
  my $self = shift;
  my $pathname = shift;

  my @file = < $pathname >;

  return @file;
}

sub get
{
  my $self = shift;
  my $dest  = shift;
  my @file = @_;

  my $this = "DTR::CP::get";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  foreach my $file (@file) {

    copy($file, $dest) or
    return $eh->error(1,errstr=>"CP GET operation failed for $file");

    my ($atime, $mtime) = (stat($file))[8,9];
    my $name = basename $file;
    my $pathname = -f $dest ? $dest : File::Spec->catfile($dest, $name);
    utime $atime, $mtime, $pathname;

  }

  return 1;

}

sub put
{
  my $self = shift;
  my $dest  = shift;
  my @file = @_;

  my $this = "DTR::CP::put";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  foreach my $file (@file) {

    copy($file, $dest) or
    return $eh->error(1,errstr=>"CP PUT operation failed for $file");

    my ($atime, $mtime) = (stat($file))[8,9];
    my $name = basename $file;
    my $pathname = -f $dest ? $dest : File::Spec->catfile($dest, $name);
    utime $atime, $mtime, $pathname;

  }

  return 1;

}

1;

=head1 NAME

DTR::CP - Data Transaction (Unix Copy)

=head1 DESCRIPTION

Sub-class of the parent DTR::TRANSACTION class for implementing the file transfer API of the data transaction (DTR) package. This module uses the File::Copy Perl implementation of Unix copy (cp) to implement the basic file transfer methods: list(), put() and get(). Please note that this class overrides the traditional functionality of a client-server transaction by mimicking the basic methods of remote file transfer.

=head1 SYNOPSIS

 $dtr = DTR::CP->new()

 @files = $dtr->list($target)
 $rc = $dtr->get($dir,@files)
 $rc = $dtr->put($dir,@files)

=over 14

=item new()

Constructor for instantiating a data transaction object using a local file copy protocol.

=item list()

List method for retrieving a file listing. $target is a directory, filename or glob string describing the source of the files to be listed.

=item get()

Get (copy) the list of files (@files) and store them in the local directory ($dir). get() returns 1 for success. See B<ERROR HANDLING> section below.

=item put()

Put (copy) the files listed in (@files) in the directory specified by $dir. put() returns 1 for success. See B<ERROR HANDLING> section below.

=item $dtr

Data transaction object.

=item $target

Directory, filename or glob string describing the file(s).

=item $dir

Directory name (i.e. path).

=item @files

List of filenames.

=item $rc

Return code. 1: success, 0 or undef: failure (see B<ERROR HANDLING> section below).

=back

=head1 SEE ALSO

DTR, DTR::TRANSACTION, DTR::SCP, DTR::FTP, DTR::DMF, DTR::NULL

=head1 NOTES

=over 4

=item 1.

The $dtr object implements the API defined for the data transaction package (DTR) and inherits the methods of the parent class, DTR::TRANSACTION.

=item 2.

The transfer protocol is typically defined in a data transaction (.dtr) file and automatically determined by invoking the constructor of the generic container class, DTR. Type B"perldoc DTR" for more information.

=back

=head1 ERROR HANDLING

Exceptions can be managed using the ERROR::Handler package. Type "perldoc ERROR::Handler" for more information. Examples follow:

 $eh = ERROR::Handler->new(\*STDERR, "myApp");
 $dtr->put($dir,@files) or die $eh->errstr;

 $rc = $dtr->get($dir,@files);
 die $eh->errstr if $eh->isError();

=head1 AUTHORS

Joseph V. Ardizzone

=head1 COPYRIGHT

This software is the property of the National Aeronautics and Space
Administration (NASA) and is subject to the regulations contained in the NASA
Procedural Requirements document NPR 2210.1C managed by the Office of the Chief
Technologist.

=cut
