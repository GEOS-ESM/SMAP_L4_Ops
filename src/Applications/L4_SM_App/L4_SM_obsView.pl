#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;
  use feature qw(switch say);
  use Data::UUID;

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

  use CONFIG;
  use FIND;
  use DTR;
  use DMGR;
  use FIND;
  use LOG::Handler;
  use METADATA;
  use L4_SM::LOG;
  use L4_SM::OBSLOG;
  use L4_SM::AUP::QA;
  use L4_SM::GPH::QA;
  use L4_SM::LMC::QA;
  use ERROR::Handler;

# Retrieve run-time input
# parameters
# =======================

  @ARGV != 3 and exit 1;

  my $path  = $ARGV[0];
  my $sdate = $ARGV[1];
  my $edate = $ARGV[2];

  $path = File::Spec->catdir($path, "runModel", "output", "ldas");
  my $config = CONFIG->new($path);

# Get observation counts
# ======================

  my (@walltime, @spl1ctb, @spl2smap, @spl2sma);
  my $tstart = CLOCK->new(DATE=>$sdate,TIME=>0);
  my $tend   = CLOCK->new(DATE=>$edate,TIME=>0);

  for (my $time = $tstart, my $t=0; $time <= $tend; $time += 86400, $t+=1) {

    my $date = $time->strftime("%Y%m%d");
    my $input = DMGR->new($path,%$config,DATE=>$date);
    DTR: my $dtr = $input->{RC_OUT};
    $dtr or next;

#   Retrieve the OBSLOG file information
#   ====================================

    my @files = $dtr->files();
    my $file  = (grep /ldas_obslog.*\.txt$/, @files)[0];
    my $pathname = $config->time_interp($file,$time);

    -f $pathname or next;

    my $obslog = L4_SM::OBSLOG->new($pathname);
    $obslog->read($pathname);

#   Retrieve information on the SMAP
#   observations used by the LDAS for
#   the specified date/time.
#   =================================

    my $hash;
    my ($obs_A, $obs_D);

    SPL1CTB:

    $hash = $obslog->{SMAP_L1C_Tbh_D} // {};
    $obs_D = $hash->{OBS_COUNT} // [];
    $hash = $obslog->{SMAP_L1C_Tbh_A} // {};
    $obs_A = $hash->{OBS_COUNT} // [];

    foreach my $count (@$obs_D, @$obs_A) { $spl1ctb[$t] += $count }

    $hash = $obslog->{SMAP_L1C_Tbv_D} // {};
    $obs_D = $hash->{OBS_COUNT} // [];
    $hash = $obslog->{SMAP_L1C_Tbv_A} // {};
    $obs_A = $hash->{OBS_COUNT} // [];

    foreach my $count (@$obs_D, @$obs_A) { $spl1ctb[$t] += $count }

    SPL2SMAP:

    $hash = $obslog->{SMAP_L2AP_Tbh_D} // {};
    $obs_D = $hash->{OBS_COUNT} // [];

    foreach my $count (@$obs_D) { $spl2smap[$t] += $count }

    SPL2SMA:

    $hash = $obslog->{SMAP_L2A_Tbh_D} // {};
    $obs_D = $hash->{OBS_COUNT} // [];

    foreach my $count (@$obs_D) { $spl2sma[$t] += $count }

    JOBTIME:

    $file  = (grep /ldas_log.*\.txt$/, @files)[0];
    $pathname = $config->time_interp($file,$time);

    -f $pathname or next;

    open my $fh, "<$pathname";

    while (<$fh>) {

      chomp;

      /Walltime Used/ or next;

      /: (\d+):(\d+):(\d+)/ or next;

      my $hours = $1 + $2/60 + $3/3600;
      push @walltime, $hours;

    }

    close $fh;

  }

  writeHeader();

  my $scale = 4.0 / 2000000.0;
  writeLineMeta(0,1,1.0,@walltime);
  writeLineMeta(1,2,$scale,@spl1ctb);
  writeLineMeta(2,3,$scale,@spl2smap);
  
exit 0;

sub writeLineMeta { my $set = shift;
                    my $color = shift;
                    my $scale = shift;
                    my @values = scalar(@_) ? @_ : ();

  print "\@    s$set hidden false\n";
  print "\@    s$set type xy\n";
  print "\@    s$set symbol 1\n";
  print "\@    s$set symbol size 0.490000\n";
  print "\@    s$set symbol color $color\n";
  print "\@    s$set symbol pattern 1\n";
  print "\@    s$set symbol fill color $color\n";
  print "\@    s$set symbol fill pattern 1\n";
  print "\@    s$set symbol linewidth 1.0\n";
  print "\@    s$set symbol linestyle 1\n";
  print "\@    s$set symbol char 65\n";
  print "\@    s$set symbol char font 4\n";
  print "\@    s$set symbol skip 0\n";
  print "\@    s$set line type 1\n";
  print "\@    s$set line linestyle 1\n";
  print "\@    s$set line linewidth 3.0\n";
  print "\@    s$set line color $color\n";
  print "\@    s$set line pattern 1\n";
  print "\@    s$set baseline type 0\n";
  print "\@    s$set baseline off\n";
  print "\@    s$set dropline off\n";
  print "\@    s$set fill type 0\n";
  print "\@    s$set fill rule 0\n";
  print "\@    s$set fill color $color\n";
  print "\@    s$set fill pattern 1\n";
  print "\@    s$set avalue off\n";
  print "\@    s$set avalue type 2\n";
  print "\@    s$set avalue char size 1.000000\n";
  print "\@    s$set avalue font 4\n";
  print "\@    s$set avalue color $color\n";
  print "\@    s$set avalue rot 0\n";
  print "\@    s$set avalue format general\n";
  print "\@    s$set avalue prec 3\n";
  print "\@    s$set avalue prepend \"\"\n";
  print "\@    s$set avalue append \"\"\n";
  print "\@    s$set avalue offset 0.000000 , 0.000000\n";
  print "\@    s$set errorbar on\n";
  print "\@    s$set errorbar place both\n";
  print "\@    s$set errorbar color 2\n";
  print "\@    s$set errorbar pattern 1\n";
  print "\@    s$set errorbar size 2.000000\n";
  print "\@    s$set errorbar linewidth 1.0\n";
  print "\@    s$set errorbar linestyle 1\n";
  print "\@    s$set errorbar riser linewidth 1.0\n";
  print "\@    s$set errorbar riser linestyle 1\n";
  print "\@    s$set errorbar riser clip off\n";
  print "\@    s$set errorbar riser clip length 0.100000\n";

  print "\@target G0.S$set\n";
  print "\@type xy\n";

  foreach my $x (0..$#values) { my $y = $values[$x] * $scale; print "$x $y\n" }

  print "&\n";

  return 1;

}

sub writeHeader { my $subtitle = shift;


  print "# Grace project file\n";
  print "#\n";
  print "\@version 50109\n";
  print "\@page size 792, 612\n";
  print "\@page scroll 5\%\n";
  print "\@page inout 5\%\n";
  print "\@link page off\n";
  print "\@map font 0 to \"Times-Roman\", \"Times-Roman\"\n";
  print "\@map font 2 to \"Times-Italic\", \"Times-Italic\"\n";
  print "\@map font 1 to \"Times-Bold\", \"Times-Bold\"\n";
  print "\@map font 3 to \"Times-BoldItalic\", \"Times-BoldItalic\"\n";
  print "\@map font 4 to \"Helvetica\", \"Helvetica\"\n";
  print "\@map font 6 to \"Helvetica-Oblique\", \"Helvetica-Oblique\"\n";
  print "\@map font 5 to \"Helvetica-Bold\", \"Helvetica-Bold\"\n";
  print "\@map font 7 to \"Helvetica-BoldOblique\", \"Helvetica-BoldOblique\"\n";
  print "\@map font 8 to \"Symbol\", \"Symbol\"\n";
  print "\@map font 9 to \"ZapfDingbats\", \"ZapfDingbats\"\n";
  print "\@map color 0 to (255, 255, 255), \"white\"\n";
  print "\@map color 1 to (0, 0, 0), \"black\"\n";
  print "\@map color 2 to (255, 0, 0), \"red\"\n";
  print "\@map color 3 to (0, 255, 0), \"green\"\n";
  print "\@map color 4 to (0, 0, 255), \"blue\"\n";
  print "\@map color 5 to (255, 255, 0), \"yellow\"\n";
  print "\@map color 6 to (188, 143, 143), \"brown\"\n";
  print "\@map color 7 to (220, 220, 220), \"grey\"\n";
  print "\@map color 8 to (148, 0, 211), \"violet\"\n";
  print "\@map color 9 to (0, 255, 255), \"cyan\"\n";
  print "\@map color 10 to (255, 0, 255), \"magenta\"\n";
  print "\@map color 11 to (255, 165, 0), \"orange\"\n";
  print "\@map color 12 to (114, 33, 188), \"indigo\"\n";
  print "\@map color 13 to (103, 7, 72), \"maroon\"\n";
  print "\@map color 14 to (64, 224, 208), \"turquoise\"\n";
  print "\@map color 15 to (0, 139, 0), \"green4\"\n";
  print "\@reference date 0\n";
  print "\@date wrap on\n";
  print "\@date wrap year 1900\n";
  print "\@default linewidth 4.0\n";
  print "\@default linestyle 1\n";
  print "\@default color 3\n";
  print "\@default pattern 1\n";
  print "\@default font 4\n";
  print "\@default char size 1.000000\n";
  print "\@default symbol size 1.000000\n";
  print "\@default sformat \"\%16.8g\"\n";
  print "\@background color 0\n";
  print "\@page background fill on\n";
  print "\@timestamp off\n";
  print "\@timestamp 0.03, 0.03\n";
  print "\@timestamp color 1\n";
  print "\@timestamp rot 0\n";
  print "\@timestamp font 4\n";
  print "\@timestamp char size 1.000000\n";
  print "\@timestamp def \"Wed Oct 15 17:48:30 2014\"\n";
  print "\@r0 off\n";
  print "\@link r0 to g0\n";
  print "\@r0 type above\n";
  print "\@r0 linestyle 1\n";
  print "\@r0 linewidth 1.0\n";
  print "\@r0 color 1\n";
  print "\@r0 line 0, 0, 0, 0\n";
  print "\@r1 off\n";
  print "\@link r1 to g0\n";
  print "\@r1 type above\n";
  print "\@r1 linestyle 1\n";
  print "\@r1 linewidth 1.0\n";
  print "\@r1 color 1\n";
  print "\@r1 line 0, 0, 0, 0\n";
  print "\@r2 off\n";
  print "\@link r2 to g0\n";
  print "\@r2 type above\n";
  print "\@r2 linestyle 1\n";
  print "\@r2 linewidth 1.0\n";
  print "\@r2 color 1\n";
  print "\@r2 line 0, 0, 0, 0\n";
  print "\@r3 off\n";
  print "\@link r3 to g0\n";
  print "\@r3 type above\n";
  print "\@r3 linestyle 1\n";
  print "\@r3 linewidth 1.0\n";
  print "\@r3 color 1\n";
  print "\@r3 line 0, 0, 0, 0\n";
  print "\@r4 off\n";
  print "\@link r4 to g0\n";
  print "\@r4 type above\n";
  print "\@r4 linestyle 1\n";
  print "\@r4 linewidth 1.0\n";
  print "\@r4 color 1\n";
  print "\@r4 line 0, 0, 0, 0\n";
  print "\@g0 on\n";
  print "\@g0 hidden false\n";
  print "\@g0 type XY\n";
  print "\@g0 stacked false\n";
  print "\@g0 bar hgap 0.000000\n";
  print "\@g0 fixedpoint off\n";
  print "\@g0 fixedpoint type 0\n";
  print "\@g0 fixedpoint xy 0.000000, 0.000000\n";
  print "\@g0 fixedpoint format general general\n";
  print "\@g0 fixedpoint prec 6, 6\n";
  print "\@with g0\n";
  print "\@    world xmin 0\n";
  print "\@    world xmax 18\n";
  print "\@    world ymin 0\n";
  print "\@    world ymax 4\n";
  print "\@    stack world 0, 0, 0, 0\n";
  print "\@    znorm 1\n";
  print "\@    view xmin 0.129490\n";
  print "\@    view xmax 1.035922\n";
  print "\@    view ymin 0.250000\n";
  print "\@    view ymax 0.850000\n";
# print "\@    title \"L4_SM SPS Processing\"\n";
# print "\@    title font 4\n";
# print "\@    title size 1.500000\n";
# print "\@    title color 1\n";
# print "\@    subtitle \"$subtitle\"\n";
# print "\@    subtitle font 4\n";
# print "\@    subtitle size 1.000000\n";
# print "\@    subtitle color 1\n";
  print "\@    xaxes scale Normal\n";
  print "\@    yaxes scale Normal\n";
  print "\@    xaxes invert off\n";
  print "\@    yaxes invert off\n";
  print "\@    xaxis  on\n";
  print "\@    xaxis  type zero false\n";
  print "\@    xaxis  offset 0.000000 , 0.000000\n";
  print "\@    xaxis  bar off\n";
  print "\@    xaxis  bar color 1\n";
  print "\@    xaxis  bar linestyle 1\n";
  print "\@    xaxis  bar linewidth 1.0\n";
  print "\@    xaxis  label \"Day\"\n";
  print "\@    xaxis  label layout para\n";
  print "\@    xaxis  label place auto\n";
  print "\@    xaxis  label char size 1.000000\n";
  print "\@    xaxis  label font 4\n";
  print "\@    xaxis  label color 1\n";
  print "\@    xaxis  label place normal\n";
  print "\@    xaxis  tick on\n";
  print "\@    xaxis  tick major 1\n";
  print "\@    xaxis  tick minor ticks 0\n";
  print "\@    xaxis  tick default 6\n";
  print "\@    xaxis  tick place rounded true\n";
  print "\@    xaxis  tick in\n";
  print "\@    xaxis  tick major size 1.000000\n";
  print "\@    xaxis  tick major color 1\n";
  print "\@    xaxis  tick major linewidth 1.0\n";
  print "\@    xaxis  tick major linestyle 1\n";
  print "\@    xaxis  tick major grid off\n";
  print "\@    xaxis  tick minor color 1\n";
  print "\@    xaxis  tick minor linewidth 1.0\n";
  print "\@    xaxis  tick minor linestyle 1\n";
  print "\@    xaxis  tick minor grid off\n";
  print "\@    xaxis  tick minor size 0.500000\n";
  print "\@    xaxis  ticklabel on\n";
  print "\@    xaxis  ticklabel format general\n";
  print "\@    xaxis  ticklabel prec 5\n";
  print "\@    xaxis  ticklabel formula \"\"\n";
  print "\@    xaxis  ticklabel append \"\"\n";
  print "\@    xaxis  ticklabel prepend \"\"\n";
  print "\@    xaxis  ticklabel angle 0\n";
  print "\@    xaxis  ticklabel skip 0\n";
  print "\@    xaxis  ticklabel stagger 0\n";
  print "\@    xaxis  ticklabel place normal\n";
  print "\@    xaxis  ticklabel offset auto\n";
  print "\@    xaxis  ticklabel offset 0.000000 , 0.010000\n";
  print "\@    xaxis  ticklabel start type auto\n";
  print "\@    xaxis  ticklabel start 0.000000\n";
  print "\@    xaxis  ticklabel stop type auto\n";
  print "\@    xaxis  ticklabel stop 0.000000\n";
  print "\@    xaxis  ticklabel char size 0.750000\n";
  print "\@    xaxis  ticklabel font 4\n";
  print "\@    xaxis  ticklabel color 1\n";
  print "\@    xaxis  tick place both\n";
  print "\@    xaxis  tick spec type none\n";
  print "\@    yaxis  on\n";
  print "\@    yaxis  type zero false\n";
  print "\@    yaxis  offset 0.000000 , 0.000000\n";
  print "\@    yaxis  bar off\n";
  print "\@    yaxis  bar color 1\n";
  print "\@    yaxis  bar linestyle 1\n";
  print "\@    yaxis  bar linewidth 1.0\n";
  print "\@    yaxis  label \"Time (hours) / Count (x 150000)\"\n";
  print "\@    yaxis  label layout para\n";
  print "\@    yaxis  label place auto\n";
  print "\@    yaxis  label char size 1.000000\n";
  print "\@    yaxis  label font 4\n";
  print "\@    yaxis  label color 1\n";
  print "\@    yaxis  label place normal\n";
  print "\@    yaxis  tick on\n";
  print "\@    yaxis  tick major 1\n";
  print "\@    yaxis  tick minor ticks 0\n";
  print "\@    yaxis  tick default 6\n";
  print "\@    yaxis  tick place rounded true\n";
  print "\@    yaxis  tick in\n";
  print "\@    yaxis  tick major size 1.000000\n";
  print "\@    yaxis  tick major color 1\n";
  print "\@    yaxis  tick major linewidth 1.0\n";
  print "\@    yaxis  tick major linestyle 1\n";
  print "\@    yaxis  tick major grid off\n";
  print "\@    yaxis  tick minor color 1\n";
  print "\@    yaxis  tick minor linewidth 1.0\n";
  print "\@    yaxis  tick minor linestyle 1\n";
  print "\@    yaxis  tick minor grid off\n";
  print "\@    yaxis  tick minor size 0.500000\n";
  print "\@    yaxis  ticklabel on\n";
  print "\@    yaxis  ticklabel format general\n";
  print "\@    yaxis  ticklabel prec 0\n";
  print "\@    yaxis  ticklabel formula \"\"\n";
  print "\@    yaxis  ticklabel append \"\"\n";
  print "\@    yaxis  ticklabel prepend \"\"\n";
  print "\@    yaxis  ticklabel angle 0\n";
  print "\@    yaxis  ticklabel skip 0\n";
  print "\@    yaxis  ticklabel stagger 0\n";
  print "\@    yaxis  ticklabel place normal\n";
  print "\@    yaxis  ticklabel offset auto\n";
  print "\@    yaxis  ticklabel offset 0.000000 , 0.010000\n";
  print "\@    yaxis  ticklabel start type auto\n";
  print "\@    yaxis  ticklabel start 0.000000\n";
  print "\@    yaxis  ticklabel stop type auto\n";
  print "\@    yaxis  ticklabel stop 0.000000\n";
  print "\@    yaxis  ticklabel char size 0.750000\n";
  print "\@    yaxis  ticklabel font 4\n";
  print "\@    yaxis  ticklabel color 1\n";
  print "\@    yaxis  tick place both\n";
  print "\@    yaxis  tick spec type none\n";
  print "\@    altxaxis  off\n";
  print "\@    altyaxis  off\n";
  print "\@    legend off\n";
  print "\@    legend loctype view\n";
  print "\@    legend 1.06303370869, 0.825871\n";
  print "\@    legend box color 1\n";
  print "\@    legend box pattern 0\n";
  print "\@    legend box linewidth 1.0\n";
  print "\@    legend box linestyle 1\n";
  print "\@    legend box fill color 0\n";
  print "\@    legend box fill pattern 1\n";
  print "\@    legend font 4\n";
  print "\@    legend char size 0.600000\n";
  print "\@    legend color 1\n";
  print "\@    legend length 4\n";
  print "\@    legend vgap 1\n";
  print "\@    legend hgap 1\n";
  print "\@    legend invert false\n";
  print "\@    frame type 0\n";
  print "\@    frame linestyle 1\n";
  print "\@    frame linewidth 1.0\n";
  print "\@    frame color 1\n";
  print "\@    frame pattern 1\n";
  print "\@    frame background color 0\n";
  print "\@    frame background pattern 0\n";

return 1;
}
