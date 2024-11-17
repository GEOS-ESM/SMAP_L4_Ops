#!/usr/bin/csh
#=============
#PBS -S /usr/bin/csh
#PBS -N SPL4SM_2_001
#PBS -l select=1:ncpus=1:proc=west
#PBS -l walltime=1:00:00
#PBS -j oe 
#PBS -o /discover/nobackup/jardizzo/PBSoutput
#PBS -q debug
#PBS -W group_list=s1321
#PBS -W umask=022
#PBS -V

### By default, PBS executes your job from your home directory.
### However, you can use either the environment variable PBS_O_WORKDIR
### to change to the current working directory or 
### you can declare a new variable by using setenv

cd $PBS_O_WORKDIR

### or setenv my_work_dir /discover/nobackup/myuserid/workdir
### cd $my_work_dir

sleep 500

exit 0
