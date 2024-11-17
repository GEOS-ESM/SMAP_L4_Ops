
                        ################################
                        #                              #
                        #     PACKAGE ERROR HANDLER    #
                        #                              #
                        ################################

package ERROR::Handler;

use strict;

use Time::Piece;
use Time::Local;
use Time::Seconds;

my $CLIPBOARD = 0;
my $EXCEPTION = 0;
my $ERRSTR    = undef;

sub new
{

# Argument List
# =============

  INVOCANT: my $invocant = shift;
  CODE_REF: my $func = shift;
  my $handle = shift;

# Local Variables
# ---------------

  REFERENT:    my %self = ();
  STRING:      my $class = ref($invocant) || $invocant;

  my $time = gmtime();
  my $now  = $time->epoch;

  $self{HANDLE}     = $handle;
  $self{HANDLER}    = $func;
  $self{START_TIME} = $now;

  $CLIPBOARD = 0;
  $EXCEPTION = 0;
  $ERRSTR    = undef;

  bless (\%self, $class);

}

sub error
{
  my $self       = shift;
  my $error_code = shift;
  my %options    = scalar(@_) ? @_ : ();

  my $method;
  my $handler = $self->{HANDLER};
  $options{THROW} = 1 if ! exists $options{THROW};
  $options{ERROR_TYPE} = $options{ERROR_TYPE} // "error";

  foreach (ref($handler)) {

    /CODE/  and do { &$handler($self->{HANDLE},$error_code,%options); next };

    /HASH/  and do { $method = $handler->{ERROR_HANDLER};
                     $handler->$method($self->{HANDLE},$error_code,%options);
                     next;
                   };

    $error_code or last;

    /GLOB/  and do { $ERRSTR = "$self->{HANDLE} " .
                               "(\U$options{ERROR_TYPE}-$error_code) " .
                               ": $options{errstr}";

                     print $handler "$ERRSTR\n";

                     next;
                   };

    DEFAULT: print STDERR "ERROR::Handler: Unknown error handler type\n";

  }

  $options{THROW} or return undef;
  $error_code or return undef;

  $CLIPBOARD = 0;
  $EXCEPTION = $error_code;

  return undef;

}

sub throw
{ 
  my $self = shift;

  $CLIPBOARD or return undef;
  $EXCEPTION = $CLIPBOARD;
  return undef;
}

sub traceback
{

  my $self = shift;
  my $error_code = shift;

  return $self->error($error_code, THROW=>1, ERROR_TRACEBACK=>1, @_);
}

sub isError 
{ 
  my $self = shift; 

  $CLIPBOARD = $EXCEPTION;
  $EXCEPTION = 0;
  return $CLIPBOARD;
}

sub advisory
{ 
  my $self = shift;
  my $error_code = shift;

  return $self->error($error_code,THROW=>0,ERROR_TYPE=>"advisory",@_);
}

sub warning
{ 
  my $self = shift;
  my $error_code = shift;

  return $self->error($error_code, THROW=>0, ERROR_TYPE=>"warning",@_);
}

sub fatal
{ 
  my $self = shift;
  my $error_code = shift;

  return $self->error($error_code, THROW=>1, ERROR_TYPE=>"fatal", @_);
}

sub comment  { my $self = shift; return $self->error(0,COMMENT=>"$_[0]")}

sub errstr { my $self = shift; return $ERRSTR }

sub DESTROY {

  my $self = shift;

  $self->{HANDLE} or return;

  my $time = gmtime();
  my $now = $time->epoch;
  my $seconds = $now - $self->{START_TIME};

  return $self->error(0,ERROR_ELAPSED_TIME=>$seconds);

}

1;
