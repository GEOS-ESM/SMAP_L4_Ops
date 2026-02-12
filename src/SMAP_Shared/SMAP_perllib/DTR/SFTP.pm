                        #######################
                        #                     #
                        #   PACKAGE DTR::SFTP #
                        #                     #
                        #######################

package DTR::SFTP;
our @ISA = "DTR::TRANSACTION";

use strict;

use File::Basename;
use DTR::TRANSACTION;
use Net::SFTP;
use Fcntl ':mode'; # Import S_ISREG for checking regular files

use ERROR::Handler;

sub list
{

  my $self = shift;
  my $pathname = shift;
  my $path = dirname $pathname;
  my $name = basename $pathname;
  $name = glob2pat($name);

  my $user     = $self->{DTR_USER};
  my $machine  = $self->{DTR_MACHINE};

  my $this = "DTR::SFTP::list";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  my $sftp = Net::SFTP->new( "$machine", user => "$user" );

  my @remote_files = ();
  my $ls_result = $sftp->ls($path);
  if (defined $ls_result) {
    foreach my $entry (@$ls_result) {
        if (S_ISREG($entry->{a}->perm)) {
            push @remote_files, $entry->{filename};
        }
    }
  } else {
    return $eh->error(1,errstr=>"SFTP LIST operation failed for $path");
  }

  my @matching_files = grep { $_ =~ $name } @remote_files;

  my @listing = ();
  foreach my $file (@matching_files) {
      my $outfile = File::Spec->catfile($path, $file);
      push @listing, $outfile;
  }

  return @listing;

}

sub get
{
  my $self = shift;
  my $dir  = shift;
  my @file = @_;

  my $this = "DTR::SFTP::get";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  my $user     = $self->{DTR_USER};
  my $machine  = $self->{DTR_MACHINE};
  my $sftp = Net::SFTP->new( "$machine", user => "$user" );

  foreach my $file (@file) {

    my $name = basename $file;
    my $local_file = File::Spec->catfile($dir, $name);

    $sftp->get($file,$local_file) and
        return $eh->error(1,errstr=>"SFTP GET operation failed for $file");
  }

  return 1;

}

sub put
{
  my $self = shift;
  my $dir  = shift;
  my @file = @_;

  my $this = "DTR::SFTP::put";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  my $user     = $self->{DTR_USER};
  my $machine  = $self->{DTR_MACHINE};
  my $sftp     = Net::SFTP->new( "$machine", user => "$user" );

  foreach my $file (@file) {

    my $name = basename $file;
    my $remote_file = File::Spec->catfile($dir, $name);

    $sftp->put($file,$remote_file) or
        return $eh->error(1,errstr=>"SFTP PUT operation failed for $file");
  }

  return 1;

}

sub glob2pat {
    my $globstr = shift;
    my %patmap = (
        '*' => '.*',
        '?' => '.',
        '[' => '[',
        ']' => ']',
        '.' => '\.',
    );

    # Escape special regex characters except for glob wildcards
    $globstr =~ s{(.)}{
        $patmap{$1} || "$1"
    }ge;

    return '^' . $globstr . '$';
}

1;

=head1 NAME

DTR::SFTP - Data Transaction (Secure File Transfer Protocol)

=head1 DESCRIPTION

Sub-class of the parent DTR::TRANSACTION class for implementing the file transfer API of the data transaction (DTR) package. This module uses the Net::SFTP Perl implementation of the SFTP protocol to implement the basic file transfer methods: list(), put() and get().

=head1 SYNOPSIS

 $dtr = DTR::SFTP->new(DTR_MACHINE=>$machine, DTR_USER=>$user)

 @files = $dtr->list($target)
 $rc = $dtr->get($dir,@files)
 $rc = $dtr->put($dir,@files)

=over 14

=item new()

Constructor for instantiating a data transaction object using the Net::SFTP protocol.

=item list()

List method for retrieving a listing from a remote server. $target is a directory, filename or glob string describing the source of the files to be listed.

=item get()

Get the list of files (@files) from the remote machine and store them in the local directory ($dir). get() returns 1 for success. See B<ERROR HANDLING> section below.

=item put()

Put the files listed in (@files) on the remote machine in the directory specified by $dir. put() returns 1 for success. See B<ERROR HANDLING> section below.

=item $dtr

Data transaction object.

=item $machine

Server machine name (e.g. ftp.data.gov)

=item $user

User name on destination server (e.g. janedoe, anonymous).

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

DTR, DTR::TRANSACTION, DTR::FTP, DTR::SCP, DTR::CP, DTR::DMF, DTR::NULL

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
