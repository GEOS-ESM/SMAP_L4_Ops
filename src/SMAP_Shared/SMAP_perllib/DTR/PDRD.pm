                        #######################
                        #                     #
                        #  PACKAGE DTR::PDRD  #
                        #                     #
                        #######################

package DTR::PDRD;

use strict;

use File::Basename;
use File::Path;

sub new             
{                   
                                                        
# Argument List           
# -------------       
                      
  my $invocant = shift;
  my $class = ref($invocant) || $invocant;        
                      
  my $pdrd = {};
  my %options;
  
  $pdrd->{MESSAGE_TYPE} = "SHORTPDRD";
  $pdrd->{DISPOSITION}  = 1;
  $pdrd->{OPTIONS}      = \%options;
  
  $options{"FILE COUNT DISCREPENCY"}    = "INVALID FILE COUNT";

  $options{"MISSING PARAMETER"}         = "MISSING OR INVALID " .
                                          "ORIGINATING SYSTEM PARAMETER";

  $options{"FILE_ID DISCREPENCY"}       = "MISSING OR INVALID " .
                                          "ORIGINATING SYSTEM PARAMETER";

  $options{"DIRECTORY_ID DISCREPENCY"}  = "MISSING OR INVALID " .
                                          "ORIGINATING SYSTEM PARAMETER";

  $options{"UNEXPECTED VALUE"}          = "MISSING OR INVALID " .
                                          "ORIGINATING SYSTEM PARAMETER";

  bless($pdrd, $class);
}

sub keys {

  my $self = shift;
  return qw/MESSAGE_TYPE DISPOSITION/;
}

sub write
{
  my $self = shift;
  my $pathname = shift;
  my $disposition = shift;

  my $path = dirname $pathname;
  my $options = $self->{OPTIONS};
  $self->{DISPOSITION} = $options->{$disposition};

  if (! -d $path) {mkpath $path or die "Unable to create PDRD directory\n"}
  open WRITER, ">$pathname" or die "Unable to write PDRD\n";

  foreach my $key ($self->keys()) { print WRITER "$key = $self->{$key};\n"; }

  close WRITER;

  return 1;

}

1;
