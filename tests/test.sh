#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         test.sh           
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
################################################################################
#  Set environment.
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
export machine=${machine:-WCOSS}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')
#if [ $machine = ZEUS ] ; then
# module load intel
# module load mpt
#fi
#  Command line arguments.
export APRUNC=${APRUNC:-""}
#export SIGINP=${1:-${SIGINP:-NULL}}
#export SIGINP=${1:-${SIGINP:-/global/noscrub/Hang.Lei/para_gfs/oldprgfs4nems/nemsgsmsig/sigf03.gdas.2014063018}}
#export SIGINP=${1:-${SIGINP:-/global/noscrub/Hang.Lei/para_gfs/oldprgfs4nems/sigfiles/sigf03.gdas.2014063018}}
export SIGINP=${1:-${SIGINP:-./testdataset}}
#export SIGINP=${1:-${SIGINP:-/global/save/Hang.Lei/para_gfs/forruss/}}
#export SIGINP=${1:-${SIGINP:-/global/noscrub/Kate.Howard/para_gfs/data_4_gfs4nems/chgres_nemsio_2015033100_T1534/gfsanl.gfs.2015033100}}
#export SIGINP=${1:-${SIGINP:-/global/save/Hang.Lei/para_gfs/sigout.t574}}
#export SFCINP=${2:-${SFCINP:-/global/noscrub/Hang.Lei/para_gfs/oldprgfs4nems/sfcf03.gdas.2015011212}}
export SFCINP=${2:-${SFCINP:-NULL}}
#export SFCINP=${2:-${SFCINP:-/global/save/Hang.Lei/para_gfs/forruss/sfcanl.gdas.2015040906}}
#export SFCINP=${2:-${SFCINP:-$2}}
export NSTINP=${12:-${NSTINP:-NULL}}
#export NSTINP=${12:-${NSTINP:-$12}}
#export OUTTYP=${10:-${OUTTYP:-2}}
export OUTTYP=${10:-${OUTTYP:-1}}
export SIGOUT=${3:-${SIGOUT:-NULL}}
#export SIGOUT=${3:-${SIGOUT:-$3}}
#export GFSOUT=${9:-${GFSOUT:-gfsout}}
export GFSOUT=${9:-${GFSOUT:-gfsout}}
export SFCOUT=${4:-${SFCOUT:-sfcout}}
#export SFCOUT=${4:-${SFCOUT:-$4}}
#export NSTOUT=${13:-${NSTOUT:-nstout}}
export NSTOUT=${13:-${NSTOUT:-$13}}
#export JCAP=${5:-${JCAP:-126}}
export JCAP=${5:-${JCAP:-574}}
#export LEVS=${6:-${LEVS:?}}
export LEVS=${6:-${LEVS:-64}}
#export LONB=${7:-${LONB:?}}
#export LONB=${7:-${LONB:-256}}
export LONB=${7:-${LONB:-1152}}
#export LONB=${7:-${LONB:-3072}}
#export LONB=${7:-${LONB:-512}}
#export LATB=${8:-${LATB:?}}
#export LATB=${8:-${LATB:-128}}
export LATB=${8:-${LATB:-576}}
#export LATB=${8:-${LATB:-1536}}
#export LATB=${8:-${LATB:-256}}
#export IDRT=${11:-${IDRT:-4}}
export IDRT=${11:-${IDRT:-0}}
#  Directories.
export NWPROD=${NWPROD:-/nwprod}
#export FIXSUBDA=${FIXSUBDA:-fix/fix_am}
export FIXSUBDA=${FIXSUBDA:-gsm.v12.0.1/fix/fix_am}
export FIXGLOBAL=${FIXGLOBAL:-$NWPROD/$FIXSUBDA}
export EXECGLOBAL=${EXECGLOBAL:-$NWPROD/exec}
export DATA=${DATA:-$(pwd)}
#  Filenames.
export XC=${XC}
#export CHGRESEXEC=${CHGRESEXEC:-/global/save/Hang.Lei/gfs4nems/para/sorc/global_chgres.fd/global_chgres$XC}
export CHGRESEXEC=${CHGRESEXEC:-./code/global_chgres$XC}
#export CHGRESEXEC=${CHGRESEXEC:-/global/save/Hang.Lei/nems/chres/Moorthi_chgres_code/global_chgres$XC}
export OROGRAPHY=${OROGRAPHY:-${FIXGLOBAL}/global_orography.t${JCAP}.${LONB}.${LATB}.grb}
#export OROGRAPHY_UF=${OROGRAPHY_UF:-${FIXGLOBAL}/global_orography_uf.t${JCAP}.$LONB.$LATB.grb}
#export SIGLEVEL=$SIGLEVEL
#export SIGLEVEL=${SIGLEVEL:-${FIXGLOBAL}/global_siglevel.l${LEVS}.txt}
export O3CLIM=${O3CLIM:-${FIXGLOBAL}/global_o3clim.txt}
export SLMASK=${SLMASK:-${FIXGLOBAL}/global_slmask.t${JCAP}.${LONB}.${LATB}.grb}
export FNGLAC=${FNGLAC:-${FIXGLOBAL}/global_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXGLOBAL}/global_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXGLOBAL}/cfs_oi2sst1x1monclim19822001.grb}
export FNSNOC=${FNSNOC:-${FIXGLOBAL}/global_snoclim.1.875.grb}
export FNZORC=${FNZORC:-sib}
export FNALBC=${FNALBC:-${FIXGLOBAL}/global_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXGLOBAL}/cfs_ice1x1monclim19822001.grb}
export FNTG3C=${FNTG3C:-${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXGLOBAL}/global_vegtype.1x1.grb}
export FNSOTC=${FNSOTC:-${FIXGLOBAL}/global_soiltype.1x1.grb}
export FNSMCC=${FNSMCC:-${FIXGLOBAL}/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb}
export FNVMNC=${FNVMNC:-${FIXGLOBAL}/global_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXGLOBAL}/global_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXGLOBAL}/global_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXGLOBAL}/global_snoalb.1x1.grb}
export FNMSKH=${FNMSKH:-${FIXGLOBAL}/seaice_newland.grb}
export LANDICE_OPT=${LANDICE_OPT:-2}
export CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-3}
export LONSPERLAT=${LONSPERLAT:-${FIXGLOBAL}/global_lonsperlat.t${JCAP}.${LONB}.${LATB}.txt}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export NTRAC=${NTRAC:-0}
export IDVC=${IDVC:-0}
export IDSL=${IDSL:-0}
export LSOIL=${LSOIL:-0}
export IVSSFC=${IVSSFC:-0}
export CHGRESVARS=${CHGRESVARS}
#export CHGRESVARS="use_ufo=.true.,IALB=0,ntrac=3,idvc=2,idvt=21,idsl=1,IDVM=0,OUTTYP=1,GRDFMT='bin4'"
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
#ln -sf $OROGRAPHY_UF  chgres.inp.orogb_uf
ln -sf $SIGLEVEL      chgres.inp.siglevel
ln -sf $O3CLIM        chgres.inp.o3clim
ln -sf $SFCINP        chgres.inp.sfc
ln -sf $SLMASK        chgres.inp.slmgb
ln -sf $LONSPERLAT    chgres.inp.lonsperlat
ln -sf $SIGOUT        chgres.out.sig
ln -sf $GFSOUT        chgres.out.grd
ln -sf $SFCOUT        chgres.out.sfc
ln -sf $NSTOUT        chgres.out.nst

if [[ $LANDICE_OPT = 3 || $LANDICE_OPT = 4 ]]
then
 LANDICE=.false.
else
 LANDICE=.true.
fi

cat << EOF > fort.35
 &NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
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
 &veg_parameters
  veg_src_input = "sib"
  veg_src_output = "sib"
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
           LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP, IDRT=$IDRT, $CHGRESVARS
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
