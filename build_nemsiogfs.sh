#!/bin/sh

 : ${THISDIR:=$(dirname $(readlink -f -n ${BASH_SOURCE[0]}))}
 CDIR=$PWD; cd $THISDIR

 source ./Conf/Analyse_args.sh
 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh
 source ./Conf/Reset_version.sh

 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Nemsiogfs_${sys:0:5}_${sys6^}.sh
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Nemsiogfs_${sys:0:3}_${sys4^}.sh
 else
   source ./Conf/Nemsiogfs_intel_${sys^}.sh
 fi
 $CC --version &> /dev/null || {
   echo "??? NEMSIOGFS: compilers not set." >&2
   exit 1
 }
 [[ -z $NEMSIOGFS_VER || -z $NEMSIOGFS_LIB ]] && {
   echo "??? NEMSIOGFS: module/environment not set." >&2
   exit 1
 }

set -x
 nemsiogfsLib=$(basename ${NEMSIOGFS_LIB})
 nemsiogfsInc=$(basename ${NEMSIOGFS_INC})

#################
 cd src
#################

 $skip || {
#-------------------------------------------------------------------
# Start building libraries
#
 echo
 echo "   ... build nemsiogfs library ..."
 echo
   make clean LIB=$nemsiogfsLib MOD=$nemsiogfsInc
   mkdir -p $nemsiogfsInc
   MPIFFLAGS="$MPIFFLAGS -I${NEMSIO_INC} ${MODPATH}$nemsiogfsInc"
   collect_info nemsiogfs - OneLine LibInfo
   nemsiogfsInfo=nemsiogfs_info_and_log.txt
   $debg && make debug LIB=$nemsiogfsLib &> $nemsiogfsInfo \
         || make build LIB=$nemsiogfsLib &> $nemsiogfsInfo
   make message MSGSRC="$(gen_cfunction $nemsiogfsInfo OneLine LibInfo)" \
                LIB=$nemsiogfsLib
 }

 $inst && {
#
#     Install libraries and source files 
#
   $local && {
     instloc=..
     LIB_DIR=$instloc
     INCP_DIR=$instloc/include
     [ -d $INCP_DIR ] || { mkdir -p $INCP_DIR; }
     SRC_DIR=
   } || {
     [[ $instloc == --- ]] && {
       LIB_DIR=$(dirname $NEMSIOGFS_LIB)
       INCP_DIR=$(dirname $NEMSIOGFS_INC)
       SRC_DIR=$NEMSIOGFS_SRC
     } || {
       LIB_DIR=$instloc
       INCP_DIR=$instloc/include
       SRC_DIR=$instloc/src
       [[ $instloc == .. ]] && SRC_DIR=
     }
     [ -d $LIB_DIR ] || mkdir -p $LIB_DIR
     [ -d $NEMSIOGFS_INC ] && { rm -rf $NEMSIOGFS_INC; } \
                        || { mkdir -p $INCP_DIR; }
     [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
   }

   make clean LIB=
   make install LIB=$nemsiogfsLib MOD=$nemsiogfsInc \
                LIB_DIR=$LIB_DIR INC_DIR=$INCP_DIR SRC_DIR=$SRC_DIR
 }

