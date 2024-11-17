                        #################################
                        #                               #
                        #   PACKAGE DTR::DMF::Handler   #
                        #                               #
                        #################################

package DTR::DMF::Handler;

use strict;

use File::Basename;
use File::Copy;
use File::Spec;
use File::Path;
use Digest::MD5;

my $TMP_DIR;
my $DMF_DIR;
my $DMF_TAG;
my $ARCHIVED;

sub new { my $invocant = shift;
          my $path = shift;
          my $tag = scalar(@_) ? shift : 0;

  my $class  = ref($invocant) || $invocant;
  $DMF_TAG   = $tag > 2 ? 2 : $tag;

  mkpath $path or return undef if ! -d $path;
  $DMF_DIR = $path;

  my @files    = ();
  my %listing  = readList($path);
  my $filelist = File::Spec->catfile($path, "*");

  my $bytes_before = -1;
  my $bytes_after  =  1;

  while ($bytes_before != $bytes_after) {

    @files = grep { -f and -s and ! /dtr\..*/ } < $filelist >;
    $bytes_before = sumFile(@files);

    foreach my $file (@files) {

      my $name = basename $file;
      my ($inode, $size) = (stat($file))[1,7];

      my $stats = $listing{$name} // [];

      next if $stats->[0] == $inode;

      $listing{$name} = undef;

    }

    @files = grep { -f and -s and ! /dtr\..*/ } < $filelist >;
    $bytes_after = sumFile(@files);

  }

  my @unlisted = grep { ! defined $listing{$_} } (keys %listing);

  @unlisted and system("dmget @files &");

  foreach my $name (@unlisted) {

    my $file = File::Spec->catfile($path, $name);
    $listing{$name} = [DTR::DMF::Handler::statFile($file)];

  }

  $ARCHIVED = 1;

  my $entry = (values %listing)[0] // [];
  $TMP_DIR  = File::Spec->catdir($ENV{TMPDIR}, $entry->[2]);

# DTR::DMF::Handler::writeList(%listing) or return undef;

  return bless(\%listing, $class);

}

sub putFile { my $self = shift;
              my $file = shift;

  -s $file or return 0;

  my @file_stats = $self->statFile($file);

  my $name = basename $file;
  my $stats = $self->{$name} // [];

  return 1 if $stats->[1] eq $file_stats[1];

  print "Archiving: $file\n";
  copyFile($file, $DMF_DIR) or return 0;

  my $dmf_file   = File::Spec->catfile($DMF_DIR, $name);
  $self->{$name} = [$self->statFile($dmf_file)];

  return 1;

}

sub getFile { my $self = shift;
              my $dir  = shift;
              my $file = shift;

  -d $dir  or return 0;

  my $name     = basename $file;
  my $dmf_file = File::Spec->catfile($DMF_DIR, $name);
  my $tmp_file = File::Spec->catfile($TMP_DIR, $name);
  my $out_file = File::Spec->catfile($dir, $name);

  $self->{$name} or return 0;

  $ARCHIVED and do {

    my $filelist = File::Spec->catfile($DMF_DIR, "*");
    my @list     = grep { -s and -f } < $filelist >;

    system("dmget @list &");
    $self->unpack(@list);

  };

  $ARCHIVED = 0;

  my $in_file = -d $TMP_DIR ? $tmp_file : $dmf_file;
  -s $in_file or return 0;

  copyFile($in_file, $out_file) and return 1;

  return 0;

}

sub copyFile { my $src  = shift;
               my $dest = shift;

  system("/bin/cp -p $src $dest") or return 1;

  return 0;

}

sub statFile { my $self = ref($_[0]) ? shift : {};
               my $file = shift;

  -s $file or return ();

  open my $fh, "<$file";
  my $md5 = Digest::MD5->new();
  $md5->addfile($fh);
  close $fh;

  my $inode    = (stat($file))[1];
  my $checksum = $md5->hexdigest();
  my $filesize = -s $file;

  return ($inode, $checksum, $filesize);
}

sub close { my $self = shift; return undef }

sub sumFile { my @files = scalar(@_) ? @_ : ();

  my $bytes;
  foreach my $file (@files) { $bytes += -s $file }

  return $bytes;

}

sub readList { my $path = shift;

  -d $path or return ();

  my $tar_file = File::Spec->catfile($DMF_DIR, "dtr.tar");
  my $registry = File::Spec->catfile($DMF_DIR, "dtr.list");

  -s $registry or return ();

  my %registry;
  open my $fh, "<$registry";

  my %listing = ();

  while (<$fh>) {

    chomp;
    /(\S+)\s(\S+)\s(\S+)\s(\S+)$/;

#   Delete the entry if the referenced
#   file does not exist (i.e. it was
#   manually removed).

    my $file = File::Spec->catfile($DMF_DIR, "$1");
    -s $file or next if ! -s $tar_file;

#   Invalid entry if the last field
#   is undefined.

    $4 or next;

    $listing{$1} = [$2, $3, $4];

  }

  close $fh;

  return %listing;

}

sub writeList { my %listing = scalar(@_) ? @_ : ();

  -d $DMF_DIR or return 0;

  my $registry = File::Spec->catfile($DMF_DIR, "dtr.list");

  open my $fh, ">$registry" or return 0;
  foreach my $key (sort keys %listing) { print $fh "$key @{$listing{$key}}\n" }
  close $fh;

  return 1;
}

sub unpack { my $self = shift; my @files = scalar(@_) ? @_ : ();

  -d $TMP_DIR and return 1;
  grep /dtr.tar$/, @files or return 1;

  mkpath $TMP_DIR or return 0;

  foreach my $file (@files) {

    $file =~ /dtr.tar$/ and do { my $cwd = File::Spec->rel2abs(".");
                                 chdir $TMP_DIR or return 0;
                                 system("tar xvf $file") and return 0;
                                 chdir $cwd;
                                 next;
                               };

    copyFile($file, $TMP_DIR) or return 0;

  }

  return 1;

}

sub pack { my $self = shift;

  my $tar_file = File::Spec->catfile($DMF_DIR, "dtr.tar");
  my $registry = File::Spec->catfile($DMF_DIR, "dtr.list");

  -s $tar_file and return 1;
# -s $registry or  return 0;

  my $cwd = File::Spec->rel2abs(".");
  chdir $DMF_DIR or return 0;

  my @files = sort keys %$self;

  system("dmget @files &");
  system("tar cvf $tar_file @files") and return 0;
  system("tar tvf $tar_file") and return 0;

  unlink @files;

  chdir $cwd;

}

sub reset { my $self = shift;

  $DMF_DIR = undef;

  $TMP_DIR = undef if ! -d $TMP_DIR;
  $TMP_DIR or return 1;

  my $filelist = File::Spec->catfile($TMP_DIR,"*");
  unlink < $filelist >;

  rmdir $TMP_DIR or warn "$TMP_DIR: $!";

  $TMP_DIR = undef;

  return 1;

}

sub DESTROY {

  my $self = shift;

  keys %$self or return;

  my @files;
  my $tar_file = File::Spec->catfile($DMF_DIR, "dtr.tar");
  my $registry = File::Spec->catfile($DMF_DIR, "dtr.list");

  open my $fh, ">$registry";
  foreach my $key (sort keys %$self) { 

    print $fh "$key @{$self->{$key}}\n";

    my $file = File::Spec->catfile($DMF_DIR, $key);
    push @files, $file if -s $file;

  }

  close $fh;

  $DMF_TAG and do {

    if (@files) {
      system("dmtag -t $DMF_TAG @files") and warn "dmtag failed";
    }

    if (-s $tar_file) {
      system("dmtag -t $DMF_TAG $tar_file") and warn "dmtag failed";
    }

  };
  
  $self->reset();
}

1;
