#!/bin/ksh

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo log.fcst
#BSUB -eo log.fcst
#BSUB -J chgres_fcst2
#BSUB -q debug
#BSUB -M 2000
#BSUB -W 00:06
#BSUB -extsched 'CRAYLINUX[]'

set -ax

 . $MODULESHOME/init/sh
module load PrgEnv-intel

export NODES=1

export KMP_AFFINITY=disabled
export OMP_NUM_THREADS_CH=24
export APRUNC="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"

#OUTTYP=2  # orig format
OUTTYP=1  # nemsio format
#OUTTYP=0  # both

export JCAP=574
export hr=0

export BASEDIR=./
export DATA=./    

# Set chgres executable and script
export CHGRESEXEC=$BASEDIR/code/global_chgres
export CHGRESSH=$BASEDIR/code.sh

export VERBOSE=YES

# Set fixed variables
if [[ "$JCAP" = "1534" ]] ; then
   export LONB=3072
   export LATB=1536
elif [[ "$JCAP" = "574" ]] ; then
   export LONB=1152
   export LATB=576
   #export LONB=1760
   #export LATB=880
elif [[ "$JCAP" = "382" ]] ; then
   export LONB=1152
   export LATB=576
elif [[ "$JCAP" = "62" ]] ; then
   export LONB=192
   export LATB=94
fi
export LEVS=64
export LSOIL=4
export IDRT=4
export IALB=1
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=2

# fixed fields
export FIXgsm=$BASEDIR/fix/fix_am
export SLMASK=$FIXgsm/global_slmask.t$JCAP.$LONB.$LATB.rg.grb
export OROGRAPHY=$FIXgsm/global_orography.t$JCAP.$LONB.$LATB.rg.grb
export OROGRAPHY_UF=$FIXgsm/global_orography_uf.t$JCAP.$LONB.$LATB.rg.grb
export LONSPERLAT=$FIXgsm/global_lonsperlat.t$JCAP.$LONB.$LATB.txt

export CHGRESVARS="use_ufo=.true.,nst_anl=.true.,rdgrid=.true.,idvc=2,idvt=21,idsl=1,IDVM=0,OUTTYP=${OUTTYP},nopdpvv=.true."
export FNSOTC=$FIXgsm/global_soiltype.statsgo.t$JCAP.$LONB.$LATB.rg.grb
export SOILTYPE_INP=statsgo
export SOILTYPE_OUT=statsgo
export FNVETC=$FIXgsm/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb
export VEGTYPE_OUT=igbp
export VEGTYPE_INP=igbp
export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t$JCAP.$LONB.$LATB.rg.grb
export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t$JCAP.$LONB.$LATB.rg.grb
# needed for facsf and facwf
export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
export FNZORC=igbp
export FNTSFC=$FIXgsm/RTGSST.1982.2012.monthly.clim.grb
export FNAISC=$FIXgsm/CFSR.SEAICE.1982.2012.monthly.clim.grb


$CHGRESSH
rm -f chgres.*
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "***ERROR*** rc= $rc"
  exit
fi
