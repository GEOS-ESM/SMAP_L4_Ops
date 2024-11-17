#!/usr/bin/perl

  use strict;

  use File::Path;
  use File::Copy;
  use File::Basename;
  use File::Spec;
  use Getopt::Std;
  use Getopt::Long;

  use CLOCK;
  use Time::Piece;
  use Time::Local;
  use Time::Seconds;

  my $date  = $ARGV[0];
  my $clock = CLOCK->new(DATE=>$date,TIME=>0);
  my $time  = $clock->strftime("%Y-%m-%d %H:%M:%SZ");

  my @wait = ();
  my @log  = ();
  
# Read in the log entries for
# the specified date/time.
# ===========================

  open my $fh, "<log";

  while (my $line = <$fh>) {
    chomp $line;
    $line =~ /$time/ and push @log, $line;
  }

# Exit if processing is incomplete for
# the specified date/time.
# ====================================

  @log or exit 1;

  my ($is_complete) = getEventTimes("L4_C is complete",@log);
  $is_complete or exit 1;

# Retrieve the event times
# ========================

  my ($start_time)  = getEventTimes($log[0]);
  my ($end_time)    = (getEventTimes('Exiting L4_C.pl',@log))[-1];

  my @l4c_MET = (
    (getEventTimes("Executing the MET Preprocessor", @log))[-1],
    (getEventTimes("Exiting runMet", @log))[-1]
  );

  my @l4c_MODIS = (
    (getEventTimes("Executing the MODIS Preprocessor", @log))[-1],
    (getEventTimes("Exiting runModis", @log))[-1]
  );

  my @model = (
    (getEventTimes("Executing l4c_DAILY", @log))[-1],
    (getEventTimes("Exiting runDaily", @log))[-1]
  );

  my @genISO = (
    (getEventTimes("genISO", @log))[0],
    getEventTimes("Exiting genISO", @log)
  );

  my @export = (
    $genISO[-1],
    getEventTimes("Exiting export", @log)
  );

  @wait = getEventTimes('Waiting for data type: "LFO"', @log);
  push @wait, $wait[-1] + 1800 if @wait;
  my @lfo = ($start_time, $model[0]);
  @lfo = ($wait[-1], $model[0]) if @wait;

  @wait = getEventTimes('Waiting for data type: "SPL4SMGP"', @log);
  push @wait, $wait[-1] + 1800 if @wait;
  my @spl4smgp = ($start_time, $model[0]);
  @spl4smgp = ($wait[-1], $model[0]) if @wait;

  @wait = getEventTimes('Waiting for data type: "SPL3SMA"', @log);
  push @wait, $wait[-1] + 1800 if @wait;
  my @spl3sma = ($start_time, $model[0]);
  @spl3sma = ($wait[-1], $model[0]) if @wait;

  @wait = getEventTimes('Waiting for data type: "MODIS"', @log);
  push @wait, $wait[-1] + 1800 if @wait;
  my @modis = ($start_time, $model[0]);
  @modis = ($wait[-1], $model[0]) if @wait;

# Make the XMGR graph
# ===================

  my $subtitle = $clock->strftime("%Y-%m-%d");
  writeHeader($subtitle);

  writeLineMeta(0,1,3,@export);
  writeLineMeta(1,2,3,@genISO);
  writeLineMeta(2,3,3,@model);
  writeLineMeta(3,4,3,@l4c_MODIS);
  writeLineMeta(4,5,3,@l4c_MET);

  writeLineMeta(5,6,2,($start_time, $lfo[0]));
  writeLineMeta(5,7,3,@lfo);

  writeLineMeta(6,8,2,($start_time, $spl4smgp[0]));
  writeLineMeta(6,9,3,@spl4smgp);

  writeLineMeta(7,10,2,($start_time, $spl3sma[0]));
  writeLineMeta(7,11,3,@spl3sma);

  writeLineMeta(8,12,2,($start_time, $modis[0]));
  writeLineMeta(8,13,3,@modis);

exit 0;

sub getEventTimes { my $event = shift;
                    my @log   = scalar(@_) ? @_ : ($event);

  my @times = ();
  my @log_entry = ();
  my $clock = CLOCK->new();

# Locate the log entries for
# the event (default: first event).
# =================================

  @log_entry = grep /$event/, @log;

  my ($system_time) = split /\|/, $log[0];
  my $start_time = $clock->strptime($system_time,"%Y-%m-%d %H:%M:%SZ");

# Extract the event time from
# the log entries.
# ===========================

  foreach my $log_entry (@log_entry) {

    ($system_time) = split /\|/, $log_entry;
    my $time = $clock->strptime($system_time,"%Y-%m-%d %H:%M:%SZ");

    push @times, $time->epoch - $start_time->epoch;

  }

  return @times;

}

sub writeLineMeta { my $y = shift;
                    my $set = shift;
                    my $color = shift;
                    my @x = scalar(@_) ? @_ : ();

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
  print "\@    s$set line linewidth 4.0\n";
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

  foreach my $x (@x) { my $hour = $x / 3600.0; print "$hour $y\n" }

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
  print "\@    world xmax 24\n";
  print "\@    world ymin 0\n";
  print "\@    world ymax 10\n";
  print "\@    stack world 0, 0, 0, 0\n";
  print "\@    znorm 1\n";
  print "\@    view xmin 0.129490\n";
  print "\@    view xmax 1.035922\n";
  print "\@    view ymin 0.250000\n";
  print "\@    view ymax 0.850000\n";
# print "\@    title \"L4_C SPS Processing\"\n";
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
  print "\@    xaxis  label \"Elapsed Time (hours)\"\n";
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
  print "\@    yaxis  label \"\"\n";
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
  print "\@    yaxis  tick spec type both\n";
  print "\@    yaxis  tick spec 11\n";
  print "\@    yaxis  tick major 0,                0\n";
  print "\@    yaxis  ticklabel 0, \"export\"\n";
  print "\@    yaxis  tick major 1,                1\n";
  print "\@    yaxis  ticklabel 1, \"genISO\"\n";
  print "\@    yaxis  tick major 2,                2\n";
  print "\@    yaxis  ticklabel 2, \"runModel\"\n";
  print "\@    yaxis  tick major 3,                3\n";
  print "\@    yaxis  ticklabel 3, \"runModis\"\n";
  print "\@    yaxis  tick major 4,                4\n";
  print "\@    yaxis  ticklabel 4, \"runMet\"\n";
  print "\@    yaxis  tick major 5,                5\n";
  print "\@    yaxis  ticklabel 5, \"LFO\"\n";
  print "\@    yaxis  tick major 6,                6\n";
  print "\@    yaxis  ticklabel 6, \"SPL4SMGP\"\n";
  print "\@    yaxis  tick major 7,                7\n";
  print "\@    yaxis  ticklabel 7, \"SPL3SMA\"\n";
  print "\@    yaxis  tick major 8,                8\n";
  print "\@    yaxis  ticklabel 8, \"MODIS\"\n";
  print "\@    yaxis  tick major 9,                9\n";
  print "\@    yaxis  ticklabel 9, \"\"\n";
  print "\@    yaxis  tick major 10,               10\n";
  print "\@    yaxis  ticklabel 10, \"\"\n";
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
