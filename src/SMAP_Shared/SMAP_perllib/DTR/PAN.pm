                        #######################
                        #                     #
                        #  PACKAGE DTR::PAN   #
                        #                     #
                        #######################

package DTR::PAN;

use strict;

use File::Basename;
use File::Path;
use CLOCK;

sub new
{

# Argument List
# -------------

  my $invocant = shift;
  my $class = ref($invocant) || $invocant;

  my $pan = {};
  my %options;

  $pan->{MESSAGE_TYPE} = "SHORTPAN";
  $pan->{OPTIONS}      = \%options;
  $pan->{DISPOSITION}  = 1;
  $pan->{TIME_STAMP}   = 1;

  $options{"SUCCESS"}           = "SUCCESSFUL";
  $options{"TRANSFER FAILURE"}  = "FTP/KFTP FAILURE";
  $options{"MISSING PARAMETER"} = "FTP/KFTP FAILURE";
  $options{"FILE SIZE FAILURE"} = "POST-TRANSFER FILE SIZE CHECK FAILURE";
  $options{"CHECKSUM FAILURE"}  = "CHECKSUM VERIFICATION FAILURE";

  bless($pan, $class);
}

sub keys {

  my $self = shift;
  return qw/MESSAGE_TYPE DISPOSITION TIME_STAMP/;
}

sub write
{
  my $self = shift;
  my $pathname = shift;
  my $disposition = shift;

  my $path = dirname $pathname;
  my $clock = CLOCK->new();
  my $options = $self->{OPTIONS};

  $self->{DISPOSITION} = $options->{$disposition};
  $self->{TIME_STAMP}  = $clock->datetime . "Z";

  if (! -d $path) {mkpath $path or die "Unable to create PAN directory\n"}
  open WRITER, ">$pathname" or die "Unable to write PAN\n";

  foreach my $key ($self->keys()) { print WRITER "$key = $self->{$key};\n"; }

  close WRITER;

  return 1;
}

1;
