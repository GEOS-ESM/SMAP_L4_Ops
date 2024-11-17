                        #######################
                        #                     #
                        #   PACKAGE DTR::FTP  #
                        #                     #
                        #######################

package DTR::FTP;
our @ISA = "DTR::TRANSACTION";

use strict;

use File::Path;
use File::Copy;
use File::Basename;
use File::Spec;

use DTR::TRANSACTION;
use Net::FTP;
use Net::Netrc;

use ERROR::Handler;

my $ftp;
my $user;
my $machine;

sub list
{

  my $self = shift;
  my $pathname = shift;

  $self->connect() or return ();

  my @products = $ftp->ls("$pathname");

  return @products;
}

sub get { my $self = shift;
          my $dir  = shift;
          my @file = @_;

  my $this = "DTR::FTP::get";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $self->connect() or return 0;

  foreach my $file (@file) { 

    my $name = basename $file;
    my $local_file = File::Spec->catfile($dir, $name);

    $ftp->get($file,$local_file) or
        return $eh->error(1,errstr=>"FTP GET operation failed for $file");
  }

  return 1;

}

sub put { my $self = shift;
          my $dir  = shift;
          my @file = @_;

  my $this = "DTR::FTP::put";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  $self->connect() or return 0;

  foreach my $file (@file) {    

    my $name = basename $file;
    my $remote_file = File::Spec->catfile($dir, $name);

    $ftp->put($file,$remote_file) or 
        return $eh->error(1,errstr=>"FTP PUT operation failed for $file");
  }

  return 1;

}

sub connect { my $self = shift;

  my $this = "DTR::FTP::connect";
  my $eh   = ERROR::Handler->new(\*STDERR, $this);

  return 1 if $machine eq $self->{DTR_MACHINE} and
              $user    eq $self->{DTR_USER};

  $ftp->quit() if $ftp;

  $user     = $self->{DTR_USER};
  $machine  = $self->{DTR_MACHINE};
  $ftp      = Net::FTP->new("$machine", Passive=>1, Timeout=>300) or
     return $eh->error(1,errstr=>"FTP connection failed: $machine"); 

  my $session  = Net::Netrc->lookup($machine);
  my $passwd   = $session->password;

  $ftp->login("$user","$passwd") or 
     return $eh->error(2,errstr=>"FTP LOGIN failed: $user"); 

  $ftp->binary();

  return 1;

}

1;

=head1 NAME

DTR::FTP - Data Transaction (File Transfer Protocol)

=head1 DESCRIPTION

Sub-class of the parent DTR::TRANSACTION class for implementing the file transfer API of the data transaction (DTR) package. This module uses the Net::FTP Perl implementation of the FTP protocol to implement the basic file transfer methods: list(), put() and get().

=head1 SYNOPSIS

 $dtr = DTR::FTP->new(DTR_MACHINE=>$machine, DTR_USER=>$user)

 @files = $dtr->list($target)
 $rc = $dtr->get($dir,@files)
 $rc = $dtr->put($dir,@files)

=over 14

=item new()

Constructor for instantiating a data transaction object using the Net::FTP protocol.

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

DTR, DTR::TRANSACTION, DTR::SCP, DTR::CP, DTR::DMF, DTR::NULL

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
