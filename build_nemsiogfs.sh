#!/bin/sh

 (( $# == 0 )) && {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }

 sys=${1,,}
 [[ $sys == wcoss || $sys == dell || $sys == cray ||\
    $sys == theia || $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [[local]install[only]]"
   exit 1
 }
 debg=false
 inst=false
 skip=false
 local=false
 (( $# > 1 )) && {
   [[ ${2,,} == build ]] && debg=false
   [[ ${2,,} == debug ]] && debg=true
   [[ ${2,,} == install ]] && inst=true
   [[ ${2,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${2,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${2,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }
 (( $# > 2 )) && {
   [[ ${3,,} == build ]] && debg=false
   [[ ${3,,} == debug ]] && debg=true
   [[ ${3,,} == install ]] && inst=true
   [[ ${3,,} == localinstall ]] && { local=true; inst=true; }
   [[ ${3,,} == installonly ]] && { inst=true; skip=true; }
   [[ ${3,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
 }

 (( $# == 0 )) && {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [install[only]]"
   exit 1
 }
 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Nemsiogfs_${sys:0:5}_${sys6^}.sh
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Nemsiogfs_${sys:0:3}_${sys4^}.sh
 else
   source ./Conf/Nemsiogfs_intel_${sys^}.sh
 fi
 [[ -z $NEMSIOGFS_VER || -z $NEMSIOGFS_LIB ]] && {
   echo "??? NEMSIOGFS: module/environment not set."
   exit 1
 }

 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh

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
   $local && LIB_DIR=.. || LIB_DIR=$(dirname ${NEMSIOGFS_LIB})
   [ -d $LIB_DIR ] || mkdir -p $LIB_DIR
   INCP_DIR=$(dirname $NEMSIOGFS_INC)
   [ -d $NEMSIOGFS_INC ] && rm -rf $NEMSIOGFS_INC || mkdir -p $INCP_DIR
   SRC_DIR=$NEMSIOGFS_SRC
   $local && SRC_DIR=
   [ -d $SRC_DIR ] || mkdir -p $SRC_DIR
   make clean LIB=
   make install LIB=$nemsiogfsLib MOD=$nemsiogfsInc \
                LIB_DIR=$LIB_DIR INC_DIR=$INCP_DIR SRC_DIR=$SRC_DIR
 }

