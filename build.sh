#!/bin/sh

#-----------------------------------------------------
# Build the nemsiogfs library on wcoss phase 1/2.
#-----------------------------------------------------

set -x

mac=$(hostname -f)

case $mac in

  g????.ncep.noaa.gov | t????.ncep.noaa.gov)

    module purge

    module list
    module load ics/12.1
    module load nemsio/v2.2.2

    export FCOMP=ifort
    export FCFLAGS='-O -FR -I$(NEMSIO_INC)'

    make clean
    make;;

  llogin? | slogin?)

    echo COMING SOON! ;;

esac

exit
