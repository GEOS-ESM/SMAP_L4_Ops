                        #######################
                        #                     #
                        # PACKAGE SYSTEM::PBS #
                        #                     #
                        #######################

package SYSTEM::PBS;
our @ISA = "SYSTEM::SCHEDULER";

use strict;

use SYSTEM::SCHEDULER;

#******************************************************************************
sub queryBatch
#******************************************************************************
# English Name: Query Batch
# -------------
#
# Purpose: Queries the batch system and returns the fields defined by the
# -------- "qstat" command.
#
# Language: Perl
# ---------
#
# Usage: @fields = $sys->queryBatch()
# ------ 
#
# Interface:              Type   Access  Description
# ----------                     Intent
#
# $sys           SYSTEM object       IN  instantiated SYSTEM object (see
#                                        SYSTEM.pm).
#
# @fields               string      OUT  returned fields from the qstat
#                                        command (null if non-existent job).
#                                        The fields are defined as follows:
#
#                                        Job ID
#                                        Username
#                                        Queue
#                                        Jobname
#                                        Session ID
#                                        Nodes
#                                        Tasks
#                                        Required Memory
#                                        Required Time
#                                        Status (R-running, Q-waiting)
#                                        Elapsed Time
#    
# Programmer: Joseph V. Ardizzone
# ----------- (NASA Goddard Space Flight Center)
#             (Global Modeling and Assimilation Office - GMAO)
#
# Modified:       Date           Author  Description
# ---------   
#           09/19/2013      J.Ardizzone  created.
#******************************************************************************
{
  my $self = shift;

  my (@output, @fields);
  my ($match, $jobID, $jobName);

  return () if ! $self->{JOBNAME};

  @output = `qstat`;
  ($match) = grep /$self->{JOBNAME}/, @output;
  return () if ! $match;

  ($jobID, $jobName) = (split /\s+/, $match)[0,1];

  @output = `qstat -a`;
  ($match) = grep /$jobID/, @output;

  @fields = split /\s+/, $match;
  $fields[3] = $jobName;

  return @fields;

}

sub submitBatch
{

# Argument List
# -------------

  REFERENT: my $self = shift;
  STRING:   my $file = shift;

# Local Variables
# ---------------

  STRING: my $job;
  STRING: my @jobs = < $file >;

  foreach $job (@jobs) { `sbatch $job` };

}

sub queueDelete { my $self = shift;

  my $job_id = ($self->queryBatch())[0];
  $job_id or return 1;

  system "qdel $job_id";

  return 1;

}


1;
