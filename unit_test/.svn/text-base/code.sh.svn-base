#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         code.sh
# Script description:  Changes resolution of GFS files.
#
# Author:  Hang Lei modified from global_chgres
#
# Abstract: The nemsiogfs library is such a specific library to describe all GFS
#   variables in nemsio format. The unit test of it requires read and write
#   all of these variables. Therefore, we simply the global chgres code into
#   a test code to fullfill this process.
#   This script changes the resolution of the GFS files. We provide the T126
#   low resolution data as the testdataset. It changes to the high resolution
#   T574 inorder to test its function for use in high resolution.
#   The compile of the code need support of other libraries, since the test of
#   all GFS variables is a systematic work. The setting of these library (nemsio,
#   BACIO, sigio etc...) may varies based on specific machine.
#
####
################################################################################
#  Set environment.
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
export machine=${machine:-WCOSS}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
if [ $machine = WCOSS_C ]; then
  . $MODULESHOME/init/sh
  module load PrgEnv-intel intel cray-mpich
  module load prod_envir prod_util grib_util
fi
#  Command line arguments.
export APRUNC=${APRUNC:-""}
export SIGINP=${1:-${SIGINP:-./testdataset}}
export SFCINP=${2:-${SFCINP:-NULL}}
export NSTINP=${12:-${NSTINP:-NULL}}
export OUTTYP=${10:-${OUTTYP:-1}}
export SIGOUT=${3:-${SIGOUT:-NULL}}
export GFSOUT=${9:-${GFSOUT:-gfsout}}
export SFCOUT=${4:-${SFCOUT:-NULL}}
export NSTOUT=${13:-${NSTOUT:-NULL}}  # nstio format
export NSNOUT=${NSNOUT:-nsnout}         # nemsio format
export SFNOUT=${14:-${SFNOUT:-NULL}}
export JCAP=${5:-${JCAP:?}}
export LEVS=${6:-${LEVS:?}}
export LONB=${7:-${LONB:?}}
export LATB=${8:-${LATB:?}}
export IDRT=${11:-${IDRT:-4}}
#  Directories.
export global_shared_ver=${global_shared_ver:-v14.0.0}
export BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
export HOMEglobal=${HOMEglobal:-$BASEDIR/global_shared.${global_shared_ver}}
export FIXSUBDA=${FIXSUBDA:-fix/fix_am}
export FIXgsm=${FIXgsm:-$HOMEglobal/$FIXSUBDA}
export EXECgsm=${EXECgsm:-$HOMEglobal/exec}
export DATA=${DATA:-$(pwd)}
#  Filenames.
export XC=${XC}
export CHGRESEXEC=${CHGRESEXEC:-./code/global_chgres$XC}
export OROGRAPHY=${OROGRAPHY:-${FIXgsm}/global_orography.t${JCAP}.${LONB}.${LATB}.grb}
export OROGRAPHY_UF=${OROGRAPHY_UF:-${FIXgsm}/global_orography_uf.t${JCAP}.$LONB.$LATB.grb}
export SIGLEVEL=${SIGLEVEL:-${FIXgsm}/global_hyblev.l${LEVS}.txt}
if [ $LEVS = 128 ]; then
  export SIGLEVEL=${SIGLEVEL:-${FIXgsm}/global_hyblev.l${LEVS}B.txt}
fi
export O3CLIM=${O3CLIM:-${FIXgsm}/global_o3clim.txt}
export SLMASK=${SLMASK:-${FIXgsm}/global_slmask.t${JCAP}.${LONB}.${LATB}.grb}
export FNGLAC=${FNGLAC:-${FIXgsm}/global_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXgsm}/global_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXgsm}/cfs_oi2sst1x1monclim19822001.grb}
export FNSNOC=${FNSNOC:-${FIXgsm}/global_snoclim.1.875.grb}
export FNZORC=${FNZORC:-sib}
export FNALBC=${FNALBC:-${FIXgsm}/global_albedo4.1x1.grb}
export FNALBC2=${FNALBC2:-${FIXgsm}/global_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXgsm}/cfs_ice1x1monclim19822001.grb}
export FNTG3C=${FNTG3C:-${FIXgsm}/global_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXgsm}/global_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXgsm}/global_vegtype.1x1.grb}
export FNSOTC=${FNSOTC:-${FIXgsm}/global_soiltype.1x1.grb}
export FNSMCC=${FNSMCC:-${FIXgsm}/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb}
export FNVMNC=${FNVMNC:-${FIXgsm}/global_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXgsm}/global_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXgsm}/global_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXgsm}/global_snoalb.1x1.grb}
export FNMSKH=${FNMSKH:-${FIXgsm}/seaice_newland.grb}
export LANDICE_OPT=${LANDICE_OPT:-2}
export CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-3}
export SOILTYPE_INP=${SOILTYPE_INP:-"zobler"}
export SOILTYPE_OUT=${SOILTYPE_OUT:-"zobler"}
export VEGTYPE_INP=${VEGTYPE_INP:-"sib"}
export VEGTYPE_OUT=${VEGTYPE_OUT:-"sib"}
export LONSPERLAT=${LONSPERLAT:-${FIXgsm}/global_lonsperlat.t${JCAP}.${LONB}.${LATB}.txt}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export NTRAC=${NTRAC:-3}
export IALB=${IALB:-0}
export IDVC=${IDVC:-2}
export IDVT=${IDVT:-21}
export IDVM=${IDVM:-0}
export IDSL=${IDSL:-1}
export LSOIL=${LSOIL:-0}
export IVSSFC=${IVSSFC:-0}
export use_ufo=${use_ufo:-.true.}
export rdgrid=${rdgrid:-.false.}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-1024000000}
export XLSMPOPTS=${XLSMPOPTS:-"parthds=$NTHREADS:stack=$NTHSTACK"}
export KMP_STACKSIZE=${KMP_STACKSIZE:-$NTHSTACK}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
if [ $machine = IBMP6 ] ; then
  typeset -L1 l=$PGMOUT
  [[ $l = '&' ]]&&a=''||a='>'
  export REDOUT=${REDOUT:-'1>'$a}
  typeset -L1 l=$PGMERR
  [[ $l = '&' ]]&&a=''||a='>'
  export REDERR=${REDERR:-'2>'$a}
else
  export REDOUT=${REDOUT:-'1>'}
  export REDERR=${REDERR:-'2>'}
fi
if [ $OUTTYP = 1 ]; then
  export GRDFMT_CH='bin4'
elif [ $OUTTYP = 2 ]; then
  export GRDFMT_CH='grib'
fi
export GRDFMT=${GRDFMT:-${GRDFMT_CH:-'bin4'}}
export CHGRESVARS=${CHGRESVARS}
################################################################################
#  Preprocessing
$INISCRIPT
pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
################################################################################
#  Change resolution
#export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
export PGM=$CHGRESEXEC
export pgm=$PGM
$LOGSCRIPT
rm -f NULL
ln -sf $SIGINP        chgres.inp.sig
ln -sf $OROGRAPHY     chgres.inp.orogb
ln -sf $OROGRAPHY_UF  chgres.inp.orogb_uf
ln -sf $SIGLEVEL      chgres.inp.siglevel
ln -sf $O3CLIM        chgres.inp.o3clim
ln -sf $SFCINP        chgres.inp.sfc
ln -sf $NSTINP        chgres.inp.nst
ln -sf $SLMASK        chgres.inp.slmgb
ln -sf $LONSPERLAT    chgres.inp.lonsperlat
ln -sf $LONSPERLAT    chgres.inp.lpl3
ln -sf $SIGOUT        chgres.out.sig
ln -sf $GFSOUT        chgres.out.grd
ln -sf $SFCOUT        chgres.out.sfc
ln -sf $NSTOUT        chgres.out.nst
ln -sf $NSNOUT        chgres.out.nsn
ln -sf $SFNOUT        chgres.out.sfn

if [[ $LANDICE_OPT = 3 || $LANDICE_OPT = 4 ]]
then
 LANDICE=.false.
else
 LANDICE=.true.
fi

if [[ $VEGTYPE_OUT = "sib" ]]; then
 IVEGSRC=2
elif [[ $VEGTYPE_OUT = "igbp" ]]; then
 IVEGSRC=1
fi

if [[ $SOILTYPE_OUT = "zobler" ]]; then
 ISOT=0
elif [[ $SOILTYPE_OUT = "statsgo" ]]; then
 ISOT=1
fi

cat << EOF > fort.35
 &NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
  FNALBC2='${FNALBC2}'
  FNAISC='${FNAISC}'
  FNTG3C='${FNTG3C}'
  FNVEGC='${FNVEGC}'
  FNVETC='${FNVETC}'
  FNSOTC='${FNSOTC}'
  FNSMCC='${FNSMCC}'
  FNVMNC='${FNVMNC}'
  FNVMXC='${FNVMXC}'
  FNSLPC='${FNSLPC}'
  FNABSC='${FNABSC}'
  FNMSKH='${FNMSKH}'
  FNTSFA=''
  FNACNA=''
  FNSNOA=''
  LDEBUG=.false.
  LANDICE=$LANDICE
/
EOF

if [[ $SOILTYPE_INP = "zobler" ]]; then
cat << EOF > fort.81
 &soil_parameters
  soil_src_input = "zobler"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                0.404, 0.439, 0.421
  beta_input  =   4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_input  =   0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_input = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
EOF
elif [[ $SOILTYPE_INP = "statsgo" ]]; then
cat << EOF > fort.81
 &soil_parameters
  soil_src_input = "statsgo"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                0.464, -9.99, 0.200, 0.421
  beta_input  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25, 
                6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                5.25, -9.99, 4.05, 4.26
  psis_input  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                0.3548, -9.99,  0.0350, 0.0363
  satdk_input = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
EOF
fi

if [[ $SOILTYPE_OUT = "zobler" ]]; then
cat << EOF >> fort.81
  soil_src_output = "zobler"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                 0.404, 0.439, 0.421
  beta_output  =  4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_output  =  0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_output = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                 0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
/
EOF
elif [[ $SOILTYPE_OUT = "statsgo" ]]; then
cat << EOF >> fort.81
  soil_src_output = "statsgo"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99, 0.200, 0.421
  beta_output  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25, 
                 6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99, 4.05, 4.26
  psis_output  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_output = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                 3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                 1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                 1.4078e-5
/
EOF
fi

cat << EOF >> fort.81
 &veg_parameters
  veg_src_input = "${VEGTYPE_INP}"
  veg_src_output = "${VEGTYPE_OUT}"
  salp_output= -999.
  snup_output= -999.
/
 &options
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT}
  LANDICE_OPT=${LANDICE_OPT}
 /
EOF

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-${CHGRESTHREAD:-1}}

#if [ $machine = IBM ] ; then
 eval $APRUNC $CHGRESEXEC <<EOF $REDOUT$PGMOUT $REDERR$PGMERR
  &NAMCHG JCAP=$JCAP, LEVS=$LEVS, LONB=$LONB, LATB=$LATB,
           NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
           LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP,
           IDRT=$IDRT, GRDFMT=$GRDFMT, IALB=$IALB, ISOT=$ISOT,
           IVEGSRC=$IVEGSRC, $CHGRESVARS,
 /
EOF
#else
# export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-1}
# export mpi_tasks_ch=${mpi_tasks_ch:-1}
# export npe_node=${npe_node:-24}
# export pe_node=$((npe_node/OMP_NUM_THREADS))
# if [ $pe_node -gt $mpi_tasks_ch ] ; then export pe_node=$mpi_tasks_ch ; fi
# VDATE=${VDATE:-""}
#echo aprun -n$mpi_tasks_ch -N$pe_node -d$OMP_NUM_THREADS $CHGRESEXEC  >out_chgres_$VDATE
#aprun -n$mpi_tasks_ch -N$pe_node -d$OMP_NUM_THREADS $CHGRESEXEC <<EOF  >o_out_chgres_$VDATE 2>e_out_chgres_$VDATE
#&NAMCHG JCAP=$JCAP, LEVS=$LEVS, LONB=$LONB, LATB=$LATB,
#          NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
#          LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP, IDRT=$IDRT, $CHGRESVARS
#/
#EOF

#fi


export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

rm -f NULL
rm -f chgres.inp.sig chgres.inp.orogb chgres.inp.siglevel chgres.inp.o3clim
rm -f chgres.inp.sfc chgres.inp.nst chgres.inp.slmgb chgres.inp.lonsperlat
rm -f chgres.out.sig chgres.out.sfc chgres.out.nst chgres.out.grd fort.35 fort.81
rm -f chgres.out.nsn
################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
