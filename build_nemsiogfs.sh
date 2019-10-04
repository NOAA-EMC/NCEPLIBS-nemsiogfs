#!/bin/bash

 : ${THISDIR:=$(dirname $(readlink -f -n ${BASH_SOURCE[0]}))}
 CDIR=$PWD; cd $THISDIR

 source ./Conf/Analyse_args.sh
 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh
 source ./Conf/Reset_version.sh

 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Nemsiogfs_${sys:0:5}_${sys6^}.sh
   rinst=false
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Nemsiogfs_${sys:0:3}_${sys4^}.sh
   rinst=false
 else
   source ./Conf/Nemsiogfs_intel_${sys^}.sh
 fi
 $CC --version &> /dev/null || {
   echo "??? NEMSIOGFS: compilers not set." >&2
   exit 1
 }
 [[ -z ${NEMSIOGFS_VER+x} || -z ${NEMSIOGFS_LIB+x} ]] && {
   [[ -z ${libver+x} || -z ${libver} ]] && {
     echo "??? NEMSIOGFS: \"libver\" not set." >&2
     exit
   }
   NEMSIOGFS_INC=${libver}
   NEMSIOGFS_LIB=lib${libver}.a
   NEMSIOGFS_VER=v${libver##*_v}
 }

set -x
 nemsiogfsLib=$(basename ${NEMSIOGFS_LIB})
 nemsiogfsInc=$(basename ${NEMSIOGFS_INC})

#################
 cd src
#################

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

 $inst && {
#
#     Install libraries and source files 
#
   $local && {
     instloc=..
     LIB_DIR=$instloc/lib
     INCP_DIR=$instloc/include
     [ -d $LIB_DIR ] || { mkdir -p $LIB_DIR; }
     [ -d $INCP_DIR ] || { mkdir -p $INCP_DIR; }
     SRC_DIR=
   } || {
     $rinst && {
       LIB_DIR=$(dirname $NEMSIOGFS_LIB)
       INCP_DIR=$(dirname $NEMSIOGFS_INC)
       [ -d $NEMSIOGFS_INC ] && { rm -rf $NEMSIOGFS_INC; } \
                          || { mkdir -p $INCP_DIR; }
       SRC_DIR=$NEMSIOGFS_SRC
     } || {
       LIB_DIR=$instloc/lib
       INCP_DIR=$instloc/include
       [[ $instloc == .. ]] && SRC_DIR=
       NEMSIOGFS_INC=$INCP_DIR/$NEMSIOGFS_INC
       [ -d $NEMSIOGFS_INC ] && { rm -rf $NEMSIOGFS_INC; } \
                          || { mkdir -p $INCP_DIR; }
       SRC_DIR=$instloc/src
     }
     [ -d $LIB_DIR ] || mkdir -p $LIB_DIR
     [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
   }

   make clean LIB=
   make install LIB=$nemsiogfsLib MOD=$nemsiogfsInc \
                LIB_DIR=$LIB_DIR INC_DIR=$INCP_DIR SRC_DIR=$SRC_DIR
 }

