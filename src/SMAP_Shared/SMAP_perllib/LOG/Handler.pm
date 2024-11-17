
                        ################################
                        #                              #
                        #     PACKAGE ERROR HANDLER    #
                        #                              #
                        ################################

package LOG::Handler;

use File::Basename;
use File::Path;
use File::Spec;
use CLOCK;

my $SYSTEM = undef;
my $APPLICATION = undef;
my $DATE = undef;
my $TIME = undef;

sub new
{

# Argument List
# =============

  INVOCANT: my $invocant = shift;
  ANY_TYPE: my %config = scalar(@_) ? @_ : ();

# Local Variables
# ---------------

  FILE_HANDLE: my $fh;
  STRING:      my $log_file;
  REFERENT:    my %self = %config;
  STRING:      my $class = ref($invocant) || $invocant;

  $log_file = $config{L4_LOG_FILE} // $ENV{L4_LOG_FILE};

  $self{LOG_FILE} = $log_file // undef;
  $self{C} = "COMMENT";
  $self{A} = "ADVISORY";
  $self{W} = "WARNING";
  $self{E} = "ERROR";
  $self{F} = "FATAL ERROR";

  $SYSTEM       = $self{SYSTEM} // $SYSTEM;
  $APPLICATION  = $self{APPLICATION} // $APPLICATION;
  $DATE         = $self{DATE} // $DATE;
  $TIME         = $self{TIME} // $TIME;

  $self{SYSTEM}      = $SYSTEM;
  $self{APPLICATION} = $APPLICATION;
  $self{DATE}        = $DATE if defined $DATE;
  $self{TIME}        = $TIME if defined $TIME;

  bless (\%self, $class);

}

sub write
{
  my $self       = shift;
  my $fh_std     = shift;
  my $classifier = scalar(@_) ? shift : "A";
  my $code       = scalar(@_) ? shift : 0;
  my $message    = scalar(@_) ? shift : "";

  FILE_HANDLE: my $fh;

  STRING: my $logmsg;
  STRING: my $resource;
  STRING: my $message_type = $self->{$classifier};
  STRING: my $error_code = $code ? "$message_type\-$code" : $message_type;

  CLOCK:  my $now  = CLOCK->new();
  CLOCK:  my $time = CLOCK->new(%$self);
  STRING: my $systime  = $now->strftime("%Y-%m-%d %H:%M:%SZ");
  STRING: my $proctime = $time->strftime("%Y-%m-%d %H:%M:%SZ");

  $logmsg  = $systime             . "|" .
             $self->{SYSTEM}      . "|" . 
             $self->{APPLICATION} . "|" .
             $self->{HANDLE}      . "|" .
             $proctime            . "|" .
             "\@$classifier"      . "|" .
             $code                . "|" .
             $message;
  
  if ($self->{LOG_FILE}) {

    open $fh, ">>$self->{LOG_FILE}" or 
                 die "Unable to write to log: \"$log_file\": $!\n";
    print $fh "$logmsg\n";
    close $fh;

  }

  return $code if $self->{SILENT};

  if ($self->{SYSTEM} and $self->{APPLICATION}) {
    print $fh_std "$error_code issued at $systime from " ,
                   $self->{SYSTEM},"->",$self->{APPLICATION},"->",
                   $self->{HANDLE}, " while processing $proctime: $message\n";
  } else {

    print $fh_std "$error_code: $message\n";
  }
                
  return $code;

}

sub comment  { my $self = shift; return $self->write(\*STDOUT,"C",@_) }
sub advisory { my $self = shift; return $self->write(\*STDOUT,"A",@_) }
sub warning  { my $self = shift; return $self->write(\*STDOUT,"W",@_) }
sub error    { my $self = shift; return $self->write(\*STDERR,"E",@_) }
sub fatal    { my $self = shift; return $self->write(\*STDERR,"F",@_) }

1;
