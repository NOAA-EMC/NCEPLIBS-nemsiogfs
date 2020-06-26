SHELL=/bin/sh
set -x

#####################################################################################
# nemsiogfs unit test using module compile standard
# Hang Lei: Jan/10/2017
#####################################################################################
target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

set -x -e

if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 exit
fi

module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ./modulefiles/gsm_v14.1.0.$target
else
 source ./modulefiles/gsm_v14.1.0.$target          
fi
module list

curdir=`pwd`

cd ${curdir}/code
makefile.sh

