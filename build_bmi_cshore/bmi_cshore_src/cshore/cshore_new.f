      module cshore_module

        use cshore_helper_module
        !use cshore_variable_check_module
        use, intrinsic :: iso_c_binding

C         implicit none

         INTEGER, PARAMETER :: NN=5000,NB=30000,
     +                         NL=100,NURM=1408,NSPECTRUM=5000

         type cshore_vars_type

           integer :: id

           real :: dt
           real :: t
           real :: t_end

           !real :: alpha

           integer :: n_x
           integer :: n_y

           !real :: dx
           real :: dy

           real, pointer :: temperature(:,:)
           real, pointer :: temperature_tmp(:,:)

c           double precision, pointer :: ZB(:,:), MAXMWL(:)
           double precision, pointer :: MAXMWL(:)
           double precision :: TIME, TP,WKPO,ANGLE,WT(NN),FREQMIN,
     + FREQMAX, FREQNUM, TIMEBC(NB),TPBC(NB),HRMSBC(NB),
     + WSETBC(NB), SWLBC(NB),WANGBC(NB),FREQMINBC(NB),FREQMAXBC(NB),
     + FREQNUMBC(NB),
     + HRMS(NN),SIGMA(NN),H(NN),WSETUP(NN),SIGSTA(NN),
     + XBINP(NN,NL),ZBINP(NN,NL),FBINP(NN,NL),XS(NL),
     + YLINE(NL),DYLINE(NL),AGLINE(NL),
     + DXD2,DXDX,DX2,DX,XB(NN),ZB(NN,NL),FB2(NN,NL),SWLDEP(NN,NL),
     + BSLOPE(NN,NL),
     + GRAV,SQR2,SQR8,PI,TWOPI,SQRG1,SQRG2,
     + WKP,CP(NN),WN(NN),WKPSIN,STHETA(NN),CTHETA(NN),
     + FSX,FSY,FE,QWX,QWY, GBX(NN),GBY(NN),GF(NN),
     + GAMMA,QBREAK(NN),DBSTA(NN),SISMAX,ABREAK(NN),
     + DVEGSTA(NN),
     + SXXSTA(NN),TBXSTA(NN),
     + SXYSTA(NN),TBYSTA(NN),
     + EFSTA(NN),DFSTA(NN),
     + XR,ZR,SSP,
     + UMEAN(NN),USTD(NN),USTA(NN),VMEAN(NN),VSTD(NN),VSTA(NN),
     + WF,SG,SPORO1,WFSGM1,GSGM1,TANPHI,BSLOP1,BSLOP2,
     + EFFB,EFFF,D50,SHIELD,GSD50S,BLP,SLP,BLD,BEDLM,CSTABN,CSEDIA,
     + PS(NN),VS(NN),QSX(NN),QSY(NN),
     + PB(NN),GSLOPE(NN),QBX(NN),QBY(NN),Q(NN),
     + VBX(NN,NL),VSX(NN,NL),VBY(NN,NL),VSY(NN,NL),
     + VY(NN,NL),DZX(NN,NL),
     + DELT,DELZB(NN,NL),
     + RBZERO,RBETA(NN),RQ(NN),RX(NN),RY(NN),RE(NN),
     + XPINP(NN,NL),ZPINP(NN,NL),ZP(NN,NL),HP(NN,NL),
     + WNU,SNP,SDP,ALPHA,BETA1,BETA2,ALSTA,BESTA1,BESTA2,UPMEAN(NN),
     + UPSTD(NN),DPSTA(NN),QP(NN),UPMWD(NN),
     + RWH,RCREST(NL),QO(NL),QOTF,SPRATE,SLPOT,
     + W10(NB),WANGLE(NB),WINDCD(NB),TWXSTA(NB),TWYSTA(NB),
     + AWD,WDN,EWD,CWD,AQWD,BWD,AGWD,AUWD,WPM,ALSTA2,BE2,BE4,
     + PWET(NN),USWD(NN),HWD(NN),SIGWD(NN),UMEAWD(NN),USTDWD(NN),
     + VMEAWD(NN),VSTDWD(NN),HEWD(NN),UEWD(NN),QEWD(NN),H1,
     + SWLAND(NB),HWDMIN,ZW,QD,QM,DETADY(NB),DSWLDT(NB),
     + TSQO(NL),TSQBX(NL),TSQSX(NL),
     + VEGCD,VEGCDM,VEGN(NN,NL),VEGB(NN,NL),VEGD(NN,NL),
     + VEGINP(NN,NL),VEGH(NN,NL),VEGFB(NN,NL),VEGRD(NN,NL),
     + VEGRH(NN,NL),VEGZD(NN,NL),VEGZR(NN,NL),UPROOT(NN,NL),
     + EDIKE(NN,NL),ZB0(NN,NL),DSTA(NN),DSUM(NN),
     + GDINP(NN,NL),GRINP(NN,NL),GRDINP(NN,NL),GRSD(NN,NL),
     + GRSR(NN,NL),GRSRD(NN,NL), DEEB, DEEF,
     + WMINP(NN,NL),WMNODE(NN,NL),ZMESH(NN,NL),
     + ZBSTON(NN,NL),ZPSTON(NN,NL),HPSTON(NN,NL),
     + VDSAND(NN),CPSTON,EPCLAY(NN,NL),ZP0(NN,NL),RCINP(NN,NL),
     + FCINP(NN,NL),RCLAY(NN,NL),FCLAY(NN,NL),
     + DIKETOE, TZ, RUNUPKAPPA, RUNUPPHI,
     + VMEASOMEG(NSPECTRUM),VMEASSE(NSPECTRUM),VMEASWNUM(NSPECTRUM)

          integer :: IPROFL,IANGLE,IROLL,IWIND,IPERM,IOVER,IWCINT,
     + ISEDAV,IWTRAN,IVWALL(NL),ILAB,INFILT,IPOND,ITIDE,ILINE,IQYDY,
     + IVEG,ICLAY,ISMOOTH,IDISS,IFV,IWEIBULL,
     + NWAVE,NSURG,NWIND,NTIME,NBINP(NL),NPINP(NL),
     + NPT,NPE, NMEASSPEC, JMAX(NL),JSWL(NL), JR,JCREST(NL),
     + JWD,JDRY,
     + ISWLSL,JSL,JSL1,IOFLOW,JXW,JX2,NOPOND, ISTSAN,
     + mainloop_itime

C           REAL :: YVAL
C           CHARACTER(len=90) :: FINMIN
C           CHARACTER(len=90) :: VER
C           double precision, dimension(NN) :: DUMVEC
C           double precision, dimension(NB) :: QTIDE, SMDEDY
C           double precision :: KC, WKZ, WKMEAN, TMEAN
C           double precision :: URSELL,HS2H,HV2H,HV2HTOM
C           double precision :: EPS1, EPS2
C           integer :: MAXITE

           character(len=250) :: basename
           logical :: enable_cshore_stdout, enable_cshore_outputfiles,
     +               enable_varscheck_t0tend, enable_varscheck_alltime

         end type cshore_vars_type

         private :: initialize, set_boundary_conditions

      contains

c-----------------------------------------------------------------------
      function INIT(cg, BASENAME) result (status_code)

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        type (cshore_vars_type), intent (inout) :: cg
        character (len=*), intent (in) :: BASENAME
        logical :: status
        integer :: status_code
        CHARACTER VER*90 !bdj

        cg%basename = BASENAME

        status = InitializeIoConfiguration(
     +    BASENAME,
     +    cg%enable_cshore_stdout,
     +    cg%enable_cshore_outputfiles,
     +    cg%enable_varscheck_t0tend,
     +    cg%enable_varscheck_alltime)

        if (.not.status) then
          status_code = 1
          return
        end if

c        allocate(cg%ZB(NN,NL), stat=status_code)
c        if (status_code /= 0) then
c          status_code = 2
c          return
c        end if

        allocate(cg%MAXMWL(1), stat=status_code)
        if (status_code /= 0) then
          status_code = 3
          return
        end if

C     Store the first line of this CSHORE program on ODOC output file
C     ------------------------------------------------------------------
      VER = 'CSHORE-VEG-BMI, based on CSHORE USACE version, '//
     +'2014 last edit 2019-02-08 '
C      VER = 'CSHORE USACE version, 2014 last edit 2018-08-28 ' !bdj
C      VER = 'CSHORE USACE version, 2014 last edit 2016-01-14' !bdj
C      VER = 'CSHORE USACE version, 2014 last edit 2015-07-06' !bdj
C      VER = 'CSHORE USACE version, last edit 2015-03-23   ' !bdj
C      VER = 'CSHORE USACE version 2014, merged on 2015-03-12  ' !bdj
C      VER = 'CSHORE USACE version 2011, last edit 2012-08-15 ' !bdj
C      VER = '2014 CSHORE: Rolando ; 2014 February 12'
C     ------------------------------------------------------------------

C
C     Subr. 1 OPENER opens input and output files.
C     bdj 2015-03-23
C     CALL OPENER (FINMIN)
      CALL OPENER (cg,BASENAME)
C     end bdj 2015-03-23
C     Subr. 2 INPUT gets input wave and bathymetry information
C     from  the input file, FINMIN.
      CALL INPUT (cg, VER)

c     lzhu added:
c     IF cg%IDISS = 3: read the measured wave spectrum
      IF(cg%IDISS.EQ.3) THEN
         OPEN(UNIT=1129,FILE='Jadhav_Omeg_Se.txt',
     +        STATUS='OLD',ACCESS='SEQUENTIAL')

          DO 1152 IROW = 1,3
           READ (1129, *, IOSTAT=N) (cg%VMEASOMEG(INUM),
     +          INUM=1,cg%NMEASSPEC)
           READ (1129, *, IOSTAT=N) (cg%VMEASSE(INUM),
     +          INUM=1,cg%NMEASSPEC)
           READ (1129, *, IOSTAT=N) (cg%VMEASWNUM(INUM),
     +          INUM=1,cg%NMEASSPEC)
1152     CONTINUE
      ENDIF

C
C     Subr. 3 BOTTOM computes initial bathymetry at each node.
      CALL BOTTOM(cg)
C     Subr. 4 PARAM  calculates constants.
      CALL PARAM(cg)

      CALL pre_compute(cg)

        status_code = 0

      end function INIT
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
      subroutine pre_compute (cg)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      type (cshore_vars_type), intent (inout) :: cg

C     ************* START OF cg%TIME MARCHING COMPUTATION ***********
C
      cg%TIME = 0.D0
      ITIME = 0

C
C     Subr. 8 OUTPUT stores input before cg%TIME marching
      DO 1111 L=1,cg%ILINE
        if (cg%enable_cshore_outputfiles) CALL OUTPUT(cg,ITIME,L,0,1)
 1111 CONTINUE

      return
      end subroutine pre_compute
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
      subroutine compute (cg)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      type (cshore_vars_type), intent (inout) :: cg
      character (len=64) :: varscheck_label
      integer :: dump_vars_status

      REAL YVAL
      CHARACTER FINMIN*100, VER*70, BASENAME*90 !bdj
      DIMENSION DUMVEC(NN),QTIDE(NB),SMDEDY(NB)

c      DOUBLE PRECISION  UR(NURM),MMOD(NURM),UREME(NURM),MMODEME(NURM)
      DOUBLE PRECISION  KC, WKZ, WKMEAN, TMEAN
      DOUBLE PRECISION URSELL,HS2H,HV2H,HV2HTOM

C     For iteration convergence
C     EPS1 = 0.001 for depth (m), height (m) and velocity (m/s)
C     EPS2 = 0.000001 for roller volume flux (m*m/s)
C     MAXITE = 20 for maximum number of iteration
      DATA EPS1, EPS2, MAXITE/1.D-3, 1.D-6, 20/


c this is moved to pre_compute, which is placed in INIT.
cC     ************* START OF cg%TIME MARCHING COMPUTATION ***********
cC
c      cg%TIME = 0.D0
c      ITIME = 0
cC
cC     Subr. 8 OUTPUT stores input before cg%TIME marching
C      DO 1111 L=1,cg%ILINE
C        if (cg%enable_cshore_outputfiles) CALL OUTPUT(cg,ITIME,L,0,1)
C 1111 CONTINUE
C

c bmi: move do loop for itime to bmi_main or pymt
c      DO 999 ITIME=1,cg%NTIME
      ITIME=cg%mainloop_itime
C      write(*,*) 'mainloop_itime=',cg%mainloop_itime,'itime=',ITIME

      if (cg%enable_varscheck_t0tend .or.
     +  cg%enable_varscheck_alltime) then
        dump_vars_status = dump_vars(cg, "t0")
      endif

      print *, "finish init"

      DO 998 L=1, cg%ILINE
      IF(cg%ANGLE.EQ.cg%AGLINE(L)) THEN
        cg%IANGLE=0
C     normally incident waves along line L
      ELSE
        cg%IANGLE=1
C       cg%IWCINT=0
      ENDIF
C     obliquely incident waves along line L
C
C     IEND=0 during the constant wave and water level
C     IEND=1 at the end of each ITIME
C     If cg%IPOND=1, cg%QO=wave overtopping rate at ridge
C     crest and cg%QM=wave overtopping rate at landward end node cg%JMAX
        cg%QO(L)=0.D0
      IF(cg%IPOND.EQ.1) cg%QM=0.D0
C
 888  IEND=0
C
C     ..... PREPARE FOR LANDWARD MARCHING COMPUTATION
C
C     cg%SWLDEP(J,L) = still water depth at node J for the present landward
C     marching computation along cross-shore line L
C
        ICHECK=0
        DO 90 J=1,cg%JMAX(L)
          cg%SWLDEP(J,L) = cg%SWLBC(ITIME) - cg%ZB(J,L)
          IF(ICHECK.EQ.0) THEN
            IF(cg%SWLDEP(J,L).LT.0.D0)THEN
              cg%JSWL(L) = J
              ICHECK = 1
            ENDIF
          ENDIF
 90     CONTINUE
        IF(ICHECK.EQ.0) cg%JSWL(L)=cg%JMAX(L)
C     If cg%ITIDE=1 and cg%ILAB=0, computed cross-shore tidal
C     water flux QTIDE at wet node J
        IF(cg%ITIDE.EQ.1) THEN
          DO 91 J=1,cg%JMAX(L)
            SMDEDY(J)=cg%DETADY(ITIME)
C           SMDEDY(J)=SMDEDY(J)*(0.5D0+0.5D0*DTANH((cg%XB(J)-6.D0)/1.D0))
C     where the above transition function is specifically for LSTF
C     pumping system
 91       CONTINUE
          IF(cg%ILAB.EQ.0) THEN
            DO 92 J=1,cg%JMAX(L)
              IF(J.LT.cg%JSWL(L)) THEN
                QTIDE(J)=(cg%XB(cg%JSWL(L))-cg%XB(J))*cg%DSWLDT(ITIME)
              ELSE
                QTIDE(J)=0.D0
              ENDIF
 92         CONTINUE
          ENDIF
        ENDIF
C
C     If cg%IWTRAN=1 and cg%JCREST(L) is less than cg%JMAX(L), a bay exists landward of
C     the emerged structure or dune crest. The landward still water level
C     is cg%SWLAND(ITIME) for nodes J=cg%JSL,(cg%JSL+1),...,cg%JMAX if cg%ISWLSL=1
        IF(cg%IWTRAN.EQ.1) THEN
          cg%IOFLOW=0
C     If cg%ISWLSL=0, seaward SWL=landward SWL and the entire structure or dune
C     can be submerged below SWL
          IF(cg%ISWLSL.EQ.0.AND.cg%SWLBC(ITIME).GT.cg%RCREST(L)) THEN
            cg%JSL=cg%JMAX(L)
            cg%JSL1=cg%JMAX(L)
            GOTO 94
          ENDIF
C     If cg%ISWLSL=1, landward SWL must be below crest elevation to avoid seaward
C     overflow and landward overflow occurs if seaward SWL is above crest
          IF(cg%ISWLSL.EQ.1) THEN
            IF(cg%SWLAND(ITIME).GE.cg%RCREST(L)) cg%SWLAND(ITIME)=
     +          cg%RCREST(L)-1.D-2
            IF(cg%SWLBC(ITIME).GT.cg%RCREST(L)) cg%IOFLOW=1
          ENDIF
C     If cg%ISWLSL=2, no standing water exists lanward of the crest and wet and
C     dry zone extends to the end of the computation domain and overflow occurs
C     if seaward SWL is above crest
          IF(cg%ISWLSL.EQ.2) THEN
            cg%JSL=cg%JMAX(L)
            cg%JSL1=cg%JMAX(L)
            IF(cg%SWLBC(ITIME).GT.cg%RCREST(L)) cg%IOFLOW=1
            GOTO 94
          ENDIF
C     If cg%ISWLSL=0 or 1, landward SWL may intersect landward slope between nodes
C     cg%JSL and cg%JSL1
          IF(cg%JCREST(L).LT.cg%JMAX(L)) THEN
            ICHECK=0
            DO 95 J=(cg%JCREST(L)+1),cg%JMAX(L)
              DUM=cg%SWLAND(ITIME)-cg%ZB(J,L)
              IF(DUM.GT.0.D0) THEN
                cg%SWLDEP(J,L)=DUM
                IF(ICHECK.EQ.0) THEN
                  cg%JSL=J
                  cg%JSL1=cg%JSL-1
                  ICHECK=1
                ENDIF
              ENDIF
 95         CONTINUE
C     If ICHECK=0, no standing water exists lanward of crest
            IF(ICHECK.EQ.0) THEN
              cg%JSL=cg%JMAX(L)
              cg%JSL1=cg%JMAX(L)
            ENDIF
          ENDIF
C
 94       CONTINUE
        ENDIF
C
C     If cg%IPOND=1, Subr.20 PONDED finds ponded water zone
        IF(cg%IPOND.EQ.1) THEN
          CALL PONDED(cg,L)
        ENDIF
C
C.....ITERATION TO FIND cg%QO(L) ............................................
C     At beginning of each ITIME, cg%QO(L)=0.0 as specified above.
C     During each ITIME for profile evolution computation
C     with cg%IPROFL=1, converged cg%QO(L) is used as an initial quess
C     for the next profile change computation with ITEMAX=4
C     If cg%IOVER=0, cg%QO(L)=0.0 always and no iteration.
        IF(cg%IOVER.EQ.0) THEN
          ITEMAX=1
        ELSE
          ITEMAX=20
C     Computed wave overtopping rates cg%QO(L) for ITEMAX=10-30 changed
C     very little for fixed coastal structures with cg%IPROFL=0
          IF(cg%IPROFL.GE.1) ITEMAX=4
C     Computed overwashed dune profile evolutions changed very
C     little for ITEMAX=3-4.
        ENDIF
C
        ITEQO=0
 777    ITEQO=ITEQO+1
C
        cg%SIGMA(1) = cg%HRMS(1)/cg%SQR8
        cg%H(1) = cg%WSETUP(1) + cg%SWLDEP(1,L)
        cg%SIGSTA(1) = cg%SIGMA(1)/cg%H(1)
C
C     Subr.5 LWAVE returns linear wave number cg%WKP,phase velocity cg%CP(J)
C     ratio cg%WN(J) of group velocity to phase velocity and
C     sin cg%STHETA(J) and cos cg%CTHETA(J) of  wave cg%ANGLE for given
C     QDISP=water flux in dispersion relation of linear waves.
C     QDISP=0.0 is assumed for J=1 for simplicity
        QDISP=0.D0
        CALL LWAVE(cg,1,L,cg%H(1),QDISP)
C
C     Tentatively assume cg%VMEAN(1) = 0.0
        cg%VMEAN(1) = 0.D0
        VSIGT = 0.D0
C     At node J=1, no porous layer
        cg%QWX=cg%QO(L)
        IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) cg%QWX=cg%QWX+QTIDE(1)
        IF(cg%IPERM.EQ.1) THEN
          cg%UPMEAN(1)=0.D0
          cg%QP(1)=0.D0
          cg%UPSTD(1)=0.D0
          cg%DPSTA(1)=0.D0
        ENDIF
C
        IF(cg%IROLL.EQ.1) cg%RQ(1)=0.D0
        SIGMA2 =  cg%SIGMA(1)**2.D0
        cg%QWY=cg%GRAV*SIGMA2*cg%STHETA(1)/cg%CP(1)
        cg%SXXSTA(1) = SIGMA2*cg%FSX
        cg%EFSTA(1) = SIGMA2*cg%FE
        IF(cg%IANGLE.EQ.1) cg%SXYSTA(1) = SIGMA2*cg%FSY
        IF(cg%IWCINT.EQ.1) THEN
          DUM=cg%GRAV*cg%H(1)
          cg%SXXSTA(1)=cg%SXXSTA(1)+cg%QWX**2.D0/DUM
          IF(cg%IANGLE.EQ.1) cg%SXYSTA(1)=cg%SXYSTA(1)+
     +          cg%QWX*cg%QWY/DUM
        ENDIF
C
C     where roller volume flux cg%RQ(1)=0 is assumed for cg%SXXSTA(1),
C     cg%SXYSTA(1), USIGT=cg%UMEAN/SIGT, and cg%QWY
C
C     Subr.6a GBXAGF returns approximate analytical values for
C     the cg%GBX and cg%GF factors used in calculating cross-shore
C     bottom shear stress and energy dissipation.
C     Effect of cg%QWX on USIGT is neglected unless cg%IWCINT=1
C     If bottom friction coefficient is positive,
        IF(cg%FB2(1,L).GT.0.D0) THEN
          USIGT = -cg%SIGSTA(1)*cg%GRAV*cg%H(1)/cg%CP(1)/cg%CP(1)
          IF(cg%IANGLE.EQ.1) USIGT = USIGT*cg%CTHETA(1)
          DUM = cg%SIGSTA(1)*cg%CP(1)
          IF(cg%IWCINT.EQ.1) THEN
            IF (DUM.GT.1.D-10) USIGT = USIGT+cg%QWX/cg%H(1)/DUM !bdj
          ENDIF
          CALL GBXAGF(cg,cg%CTHETA(1),USIGT,cg%STHETA(1),VSIGT,
     +          cg%GBX(1),cg%GF(1))
          cg%TBXSTA(1) = cg%FB2(1,L)*cg%GBX(1)*DUM**2.D0/cg%GRAV
          cg%DFSTA(1) = cg%FB2(1,L)*cg%GF(1)*DUM**3.D0/cg%GRAV
c     lzhu changed here. 1=<cg%IVEG<=2.
c     For cg%IVEG=3, cg%TBXSTA and cg%DFSTA are solely due to bottom friction
          IF(cg%IVEG.GE.1.AND.cg%IVEG.LE.2) THEN
            DUM=cg%VEGH(1,L)
            IF(DUM.GT.cg%H(1)) DUM=cg%H(1)
            VEGCV=1.D0+DUM*cg%VEGFB(1,L)
            cg%TBXSTA(1)=VEGCV*cg%TBXSTA(1)
            cg%DFSTA(1)=VEGCV*cg%DFSTA(1)
          ENDIF
        ELSE
          cg%TBXSTA(1) = 0.D0
          cg%DFSTA(1) = 0.D0
        ENDIF
C
C     Subr.7 DBREAK computes the fraction of breaking waves and
C     the associated wave energy dissipation and returns cg%DBSTA(1).
        CALL DBREAK(cg,1, L, cg%HRMS(1), cg%H(1))
c       lzhu added on Nov. 2016
        IF (cg%IVEG.EQ.3) THEN
c          the energy dissipation due to veg is computed in DVEG()
           CALL DVEG  (cg,1, L, cg%HRMS(1), cg%H(1))
        ENDIF

C     ------------ LANDWARD MARCHING COMPUTATION -----------------------
C
C     Computation marching landward from seaward boundary, J = 1
C     Compute unknown variables at node JP1=(J+1) along line L.
        J = 0
 100    J = J + 1
        JP1 = J + 1
        ITE=0
C
c       the energy dissipation rate due to veg is added here
c       lzhu changed here:
        IF (cg%IVEG.EQ.3) THEN
           DUM=cg%DFSTA(J)+cg%DBSTA(J)+cg%DVEGSTA(J)
        ELSE
           DUM=cg%DFSTA(J)+cg%DBSTA(J)
        ENDIF
c       lzhu edits end
        IF(cg%IPERM.EQ.1) DUM=DUM+cg%DPSTA(J)
        DUM=DUM*cg%WT(J)
        DUM=(cg%EFSTA(J)-cg%DX*DUM)/cg%FE
        IF(DUM.LE.0.D0) THEN
          if (cg%enable_cshore_outputfiles) then
          WRITE(40,2901) JP1,L,cg%TIME,ITEQO,ITE,cg%QO(L)
          endif
C     Accept the computed results up to node J and end landward
C     marching computation (go to 400)
          JP1=JP1-1
          GOTO 400
        ENDIF
 2901   FORMAT(/'END OF LANDWARD MARCHING: '/
     +   'Square of SIGMA SIGTIE is negative at node ',I6,
     +   'Line=',I3 /
     +   ' TIME =', F13.3,' ITEQO=',I2,' ITE=',I2,' QO=',F13.9)
C
        SIGITE  = DSQRT(DUM)
        cg%SXXSTA(JP1) = cg%FSX*SIGITE**2.D0
        IF(cg%IROLL.EQ.1) cg%SXXSTA(JP1) = cg%SXXSTA(JP1) +
     +          cg%RX(J)*cg%RQ(J)
        IF(cg%IWCINT.EQ.1) cg%SXXSTA(JP1)=cg%SXXSTA(JP1)+
     +          cg%QWX*cg%QWX/cg%GRAV/cg%H(J)

        IF (cg%IVEG.LE.2) THEN
c begin bdj 2016-01-12
           STREAMSTRESSSTA = cg%FSX*cg%DFSTA(J)/(cg%WN(J)*cg%CP(J))
           cg%WSETUP(JP1) = cg%WSETUP(J)-(cg%SXXSTA(JP1)-cg%SXXSTA(J)+
     +   (cg%TBXSTA(J)+STREAMSTRESSSTA-cg%TWXSTA(ITIME))*cg%DX)/cg%H(J)
c end bdj 2016-01-12
        ELSEIF (cg%IVEG.EQ.3) THEN
c begin lzhu changed 2017-09-20
c The phase-averagaed depth-integrated drag (STREAMSTRESSSTA) is applied in the cross-shore momentum equation.
           STREAMSTRESSSTA = 0.0D0
           FVCWLWT         = 0.0D0
           IF (cg%VEGN(J,L).GT.0.0D0) THEN
              CALL PHASEAVEFV(cg,J,L,cg%HRMS(J),cg%H(J),STREAMSTRESSSTA,
     +          FVCWLWT)
           ENDIF

           cg%WSETUP(JP1) = cg%WSETUP(J)-(cg%SXXSTA(JP1)-cg%SXXSTA(J)+
     +   ((1.D0+cg%VEGCDM/cg%VEGCD*MIN(cg%VEGH(J,L),cg%H(J))*
     +          cg%VEGFB(J,L))*cg%TBXSTA(J)+
     +   STREAMSTRESSSTA-cg%TWXSTA(ITIME))*cg%DX)/cg%H(J)
        ENDIF
c end lzhu change 2017-09-20

        HITE = cg%WSETUP(JP1) + cg%SWLDEP(JP1,L)
C
        IF(HITE.LT.EPS1) THEN
          if (cg%enable_cshore_outputfiles) then
          WRITE(40,2902) JP1,L,cg%TIME,ITEQO,cg%QO(L)
          endif
          JP1=JP1-1
          GOTO 400
        ENDIF
 2902   FORMAT(/'END OF LANDWARD MARCHING: '/
     +   'Water depth is less than EPS1 at node ',I6,'Line=',I3 /
     +   ' TIME =',F13.3,' ITEQO =',I2,' QO =',F13.9)
C
        cg%QWX=cg%QO(L)
        IF(cg%IPERM.EQ.1) cg%QWX=cg%QO(L)-cg%QP(J)
        IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) cg%QWX=cg%QWX+QTIDE(JP1)
        IF(cg%IWCINT.EQ.1) THEN
          IF(cg%IANGLE.EQ.0) THEN
            QDISP=cg%QWX
          ELSE
            cg%QWY = HITE*cg%VMEAN(J) + cg%GRAV*SIGITE**2.D0*
     +          cg%STHETA(J)/cg%CP(J)
            IF(cg%IROLL.EQ.1) cg%QWY=cg%QWY+cg%RQ(J)*cg%STHETA(J)
            QDISP = cg%QWX*cg%CTHETA(J) + cg%QWY*cg%STHETA(J)
          ENDIF
        ENDIF
        CALL LWAVE(cg,JP1,L,HITE,QDISP)
C
        IF(cg%IANGLE.EQ.1) THEN
          DUM1 = SIGITE**2.D0
          cg%SXYSTA(JP1) = cg%FSY*DUM1
          IF(cg%IROLL.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+cg%RY(J)*
     +          cg%RQ(J)
          IF(cg%IWCINT.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+
     +          cg%QWX*cg%QWY/cg%GRAV/HITE
          DUM2 = cg%SXYSTA(JP1) - cg%SXYSTA(J)
          SIGN=cg%STHETA(JP1)*DUM2
          IF(SIGN.GT.0.D0) DUM2=0.D0
          cg%TBYSTA(JP1) = -DUM2/cg%DX + cg%TWYSTA(ITIME)
          IF(cg%ITIDE.EQ.1) cg%TBYSTA(JP1)=cg%TBYSTA(JP1)-
     +          HITE*SMDEDY(JP1)
          DUM = SIGITE/HITE
          IF(DUM.GT.cg%SISMAX) DUM = cg%SISMAX
          DUM3 = cg%CP(JP1)*cg%CP(JP1)/cg%GRAV
          cg%GBY(JP1) = cg%TBYSTA(JP1)/cg%FB2(JP1,L)/DUM3/DUM/DUM
          IF(cg%IVEG.GE.1) THEN
            DUMH=cg%VEGH(JP1,L)
            IF(DUM.GT.HITE) DUMH=HITE
            VEGCV=1.D0+DUMH*cg%VEGFB(JP1,L)
            cg%GBY(JP1)=cg%GBY(JP1)/VEGCV
          ENDIF

C     Subr. 6b VSTGBY computes VSIGT for specified cg%GBY, cg%CTHETA, USIGT
C     and cg%STHETA where effect of cg%QWX on USIGT is neglected unless cg%IWCINT=1
          USIGT = -cg%CTHETA(J)*DUM*HITE/DUM3
          IF(cg%IROLL.EQ.1) THEN
            USIGT = USIGT*(1.D0+ (cg%CP(JP1)/cg%GRAV)*cg%RQ(J)/
     +          SIGITE**2.D0)
          ENDIF
          SIGT = DUM*cg%CP(JP1)
          IF(cg%IWCINT.EQ.1) USIGT=USIGT+cg%QWX/HITE/SIGT
          CALL VSTGBY(cg,cg%CTHETA(J),USIGT,cg%STHETA(J),VSIGT,
     +                cg%GBY(JP1))
          VITE = VSIGT*SIGT
        ENDIF
C
        IF(cg%IROLL.EQ.1) THEN
          RQITE = cg%RQ(J) + cg%DX*(cg%DBSTA(J)-cg%RBETA(J)*cg%RQ(J))/
     +          cg%RE(J)
          IF(RQITE.LT.0.D0) RQITE=0.D0
        ENDIF
C
C******Begin iteration for improved Euler finite difference method****
C
        DO 200 ITE = 1, MAXITE
C
          HRMITE = SIGITE*cg%SQR8
C
          CALL DBREAK(cg,JP1,L,HRMITE, HITE)
c         lzhu added:
          IF (cg%IVEG.EQ.3) THEN
c             compute energy dissipation rate due to veg using DVEG()
              CALL DVEG  (cg,JP1,L,HRMITE, HITE)
          ENDIF
c         lzhu edits end

          cg%SIGSTA(JP1) = SIGITE/HITE
          IF(cg%SIGSTA(JP1).GT.cg%SISMAX) cg%SIGSTA(JP1) = cg%SISMAX
C
          SIGT = cg%CP(JP1)*cg%SIGSTA(JP1)
          IF(cg%IANGLE.EQ.0) THEN
            VSIGT = 0.D0
          ELSE
            VSIGT = VITE/SIGT
          ENDIF
C
C     If cg%IPERM=1, Subr.9 POFLOW computes porous flow variables.
C     cg%UPMEAN(J) = mean of horizontal discharge velocity UP
C     cg%UPSTD(J) = standard deviation of UP
C     cg%DPSTA(J) = energy dissipation rate of porous flow
          cg%QWX=cg%QO(L)
          IF(cg%IPERM.EQ.1) THEN
            PKHSIG = cg%WKP*HITE*cg%SIGSTA(JP1)
            DEDX = (cg%WSETUP(JP1) - cg%WSETUP(J))/cg%DX
            CALL POFLOW(cg,JP1,L,PKHSIG,DEDX)
            cg%QWX = cg%QO(L) - cg%QP(JP1)
          ENDIF
          IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) cg%QWX=cg%QWX+QTIDE(JP1)
C
          IF(cg%FB2(JP1,L).GT.0.D0) THEN
            DUM = cg%GRAV*HITE/cg%CP(JP1)/cg%CP(JP1)
            USIGT = -cg%CTHETA(JP1)*cg%SIGSTA(JP1)*DUM
            IF(cg%IROLL.EQ.1) THEN
              USIGT = USIGT*(1.D0+(cg%CP(JP1)/cg%GRAV)*RQITE/
     +          SIGITE**2.D0)
            ENDIF
            IF(cg%IWCINT.EQ.1) USIGT=USIGT+cg%QWX/HITE/SIGT
            CALL GBXAGF(cg,cg%CTHETA(JP1),USIGT,cg%STHETA(JP1),VSIGT,
     +      cg%GBX(JP1), cg%GF(JP1))
            cg%TBXSTA(JP1) = cg%FB2(JP1,L)*cg%GBX(JP1)*SIGT**2.D0/
     +          cg%GRAV
            cg%DFSTA(JP1) = cg%FB2(JP1,L)*cg%GF(JP1)*SIGT**3.D0/cg%GRAV
c           lzhu change to 1=<cg%IVEG<=2
c           For cg%IVEG=3, cg%TBXSTA and cg%DFSTA are solely due to bottom friction
            IF(cg%IVEG.GE.1.AND.cg%IVEG.LE.2) THEN
              DUM=cg%VEGH(JP1,L)
              IF(DUM.GT.HITE) DUM=HITE
              VEGCV=1.D0+DUM*cg%VEGFB(JP1,L)
              cg%TBXSTA(JP1)=VEGCV*cg%TBXSTA(JP1)
              cg%DFSTA(JP1)=VEGCV*cg%DFSTA(JP1)
            ENDIF
          ELSE
            cg%TBXSTA(JP1) = 0.D0
            cg%DFSTA(JP1) = 0.D0
          ENDIF
C
c         lzhu added energy dissipation rate due to veg if cg%IVEG==3
          IF (cg%IVEG.EQ.3) THEN
              DUMD = cg%DFSTA(JP1) + cg%DFSTA(J) + cg%DBSTA(JP1) +
     +          cg%DBSTA(J) +
     +           cg%DVEGSTA(JP1) + cg%DVEGSTA(J)
          ELSE
              DUMD = cg%DFSTA(JP1) + cg%DFSTA(J) + cg%DBSTA(JP1) +
     +          cg%DBSTA(J)
          ENDIF
c         lzhu edits end

          IF(cg%IPERM.EQ.1) DUMD=cg%DPSTA(JP1)+cg%DPSTA(J)+DUMD
          DUMD = DUMD*(cg%WT(J)+cg%WT(JP1))/2.D0
          DUM = (cg%EFSTA(J) - cg%DXD2*DUMD)/cg%FE
          IF(DUM.LE.0.D0) THEN
            if (cg%enable_cshore_outputfiles) then
            WRITE(40,2901) JP1, L, cg%TIME, ITEQO, ITE, cg%QO(L)
            endif
C     Accept the computed results up to node J
            JP1=JP1-1
            GOTO 400
          ELSE
            cg%SIGMA(JP1) = DSQRT(DUM)
          ENDIF
C
          cg%SXXSTA(JP1) = cg%FSX*cg%SIGMA(JP1)**2.D0
          IF(cg%IROLL.EQ.1) cg%SXXSTA(JP1)=cg%SXXSTA(JP1)+
     +          cg%RX(JP1)*RQITE
          IF(cg%IWCINT.EQ.1) cg%SXXSTA(JP1)=cg%SXXSTA(JP1)+
     +          cg%QWX*cg%QWX/cg%GRAV/HITE

          IF (cg%IVEG.LE.2) THEN
c bdj begin 2016-01-12
          STREAMSTRESSSTA = cg%FSX*(cg%DFSTA(JP1) + cg%DFSTA(J))/
     +         (cg%WN(JP1)*cg%CP(JP1) + cg%WN(J)*cg%CP(J))
          cg%WSETUP(JP1) = cg%WSETUP(J) - (2.D0* (cg%SXXSTA(JP1)-
     +          cg%SXXSTA(J)) +
     +                  cg%DX*(cg%TBXSTA(JP1)+cg%TBXSTA(J)
     +                  +2.D0*STREAMSTRESSSTA -2.D0*cg%TWXSTA(ITIME)))/
     +                  (HITE+cg%H(J))
c bdj end 2016-01-12
          ELSEIF (cg%IVEG.EQ.3) THEN
c begin lzhu changed 2017-09-21
c The phase-averaged depth-integrated drag due to veg (STREAMSTRESSSTA) is computed
c  and applied to the cross-shore momentum equation
             STREAMSTRESSSTA = 0.0D0
             FVCWLWT         = 0.0D0
             IF (cg%VEGN(J,L).GT.0.0D0) THEN
               CALL PHASEAVEFV(cg,J,L,cg%HRMS(J),cg%H(J),
     +          STREAMSTRESSSTATMP1,
     +                   FVCWLWTTMP1)
               CALL PHASEAVEFV(cg,JP1,L,HRMITE,HITE,STREAMSTRESSSTATMP2,
     +                   FVCWLWTTMP2)
               STREAMSTRESSSTA=0.5*(STREAMSTRESSSTATMP1+
     +                    STREAMSTRESSSTATMP2)
               FVCWLWT        = 0.5D0*(FVCWLWTTMP1+FVCWLWTTMP2)
             ENDIF

             cg%WSETUP(JP1)=cg%WSETUP(J)-(2.D0*(cg%SXXSTA(JP1)-
     +          cg%SXXSTA(J)) +
     +       cg%DX*((1.D0+cg%VEGCDM/cg%VEGCD*MIN(cg%VEGH(JP1,L),HITE)*
     +          cg%VEGFB(JP1,L))
     +       *cg%TBXSTA(JP1)+
     +      (1.D0+cg%VEGCDM/cg%VEGCD*MIN(cg%VEGH(J,L),cg%H(J))*
     +          cg%VEGFB(J,L))*cg%TBXSTA(J)
     +       + 2.D0*STREAMSTRESSSTA -2.D0*cg%TWXSTA(ITIME)))/
     +       (HITE+cg%H(J))
c          cg%WSETUP(JP1) = cg%WSETUP(J) - (2.D0* (cg%SXXSTA(JP1)-cg%SXXSTA(J)) +
c     +                  cg%DX*(2.0*FVCWLWT +
c     +                  2.D0*STREAMSTRESSSTA -2.D0*cg%TWXSTA(ITIME)))/
c     +                  (HITE+cg%H(J))
c end lzhu change 2017-09-21
          ENDIF

          cg%H(JP1) = cg%WSETUP(JP1) + cg%SWLDEP(JP1,L)
          cg%SIGSTA(JP1) = cg%SIGMA(JP1)/cg%H(JP1)
          IF(cg%SIGSTA(JP1).GT.cg%SISMAX) cg%SIGSTA(JP1)=cg%SISMAX
C
          IF(cg%H(JP1).LE.EPS1) THEN
            if (cg%enable_cshore_outputfiles) then
            WRITE(40,2902) JP1, L, cg%TIME, ITEQO, cg%QO(L)
            endif
            JP1 = JP1-1
            GOTO 400
          ENDIF
C
          IF(cg%IWCINT.EQ.1) THEN
            IF(cg%IANGLE.EQ.0) THEN
              QDISP = cg%QWX
            ELSE
              cg%QWY=cg%H(JP1)*VITE+
     +          cg%GRAV*cg%SIGMA(JP1)**2.D0*cg%STHETA(JP1)/cg%CP(JP1)
              IF(cg%IROLL.EQ.1) cg%QWY=cg%QWY+RQITE*cg%STHETA(JP1)
              QDISP = cg%QWX*cg%CTHETA(JP1) + cg%QWY*cg%STHETA(JP1)
            ENDIF
          ENDIF
          CALL LWAVE(cg,JP1,L,cg%H(JP1),QDISP)
C
          IF(cg%IANGLE.EQ.1) THEN
            DUM1 = cg%SIGMA(JP1)**2.D0
            cg%SXYSTA(JP1) = cg%FSY*DUM1
            IF(cg%IROLL.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+
     +          cg%RY(JP1)*RQITE
            IF(cg%IWCINT.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+
     +          cg%QWX*cg%QWY/cg%GRAV/
     +         cg%H(JP1)
            DUM2 = cg%SXYSTA(JP1) - cg%SXYSTA(J)
            SIGN=cg%STHETA(JP1)*DUM2
            IF(SIGN.GT.0.D0) DUM2=0.D0
            cg%TBYSTA(JP1) = -DUM2/cg%DX + cg%TWYSTA(ITIME)
            IF(cg%ITIDE.EQ.1) cg%TBYSTA(JP1)=cg%TBYSTA(JP1)-
     +          cg%H(JP1)*SMDEDY(JP1)
            DUM3 = cg%CP(JP1)*cg%CP(JP1)/cg%GRAV
            cg%GBY(JP1)=cg%TBYSTA(JP1)/cg%FB2(JP1,L)/DUM3/
     +         cg%SIGSTA(JP1)/cg%SIGSTA(JP1)
            IF(cg%IVEG.GE.1) THEN
              DUM=cg%VEGH(JP1,L)
              IF(DUM.GT.cg%H(JP1)) DUM=cg%H(JP1)
              VEGCV=1.0D0+DUM*cg%VEGFB(JP1,L)
              cg%GBY(JP1)=cg%GBY(JP1)/VEGCV
            ENDIF
            USIGT = -cg%CTHETA(JP1)*cg%SIGSTA(JP1)*cg%H(JP1)/DUM3
            IF(cg%IROLL.EQ.1) THEN
              USIGT=USIGT*(1.D0+(cg%CP(JP1)/cg%GRAV)*RQITE/
     +          cg%SIGMA(JP1)**2.D0)
            ENDIF
            SIGT = cg%SIGSTA(JP1)*cg%CP(JP1)
            IF(cg%IWCINT.EQ.1) USIGT=USIGT+cg%QWX/cg%H(JP1)/SIGT
            CALL VSTGBY(cg,cg%CTHETA(JP1),USIGT,cg%STHETA(JP1),VSIGT,
     +          cg%GBY(JP1))
            cg%VMEAN(JP1) = VSIGT*SIGT
          ENDIF
C
          IF(cg%IROLL.EQ.1) THEN
            DUM1 = cg%RE(JP1) + cg%DXD2*cg%RBETA(JP1)
            DUM2 = (cg%RE(J) - cg%DXD2*cg%RBETA(J))*cg%RQ(J) +
     +        cg%DXD2*(cg%DBSTA(JP1) + cg%DBSTA(J))
            cg%RQ(JP1) = DUM2/DUM1
          ENDIF
C
C     Check for convergence
C
          ESIGMA = DABS(cg%SIGMA(JP1) - SIGITE)
          EH = DABS(cg%H(JP1) - HITE)
          IF(cg%IANGLE.EQ.1) EV = DABS(cg%VMEAN(JP1) - VITE)
          IF(cg%IROLL.EQ.1) ERQ = DABS(cg%RQ(JP1) - RQITE)
          IF(ESIGMA.LT.EPS1.AND.EH.LT.EPS1) THEN
            IF(cg%IANGLE.EQ.0) THEN
              GOTO 199
            ELSE
              IF(EV.LT.EPS1) GOTO 199
              GOTO 198
            ENDIF
 199        IF(cg%IROLL.EQ.0) THEN
              GOTO 210
            ELSE
              IF(ERQ.LT.EPS2) GOTO 210
              GOTO 198
            ENDIF
          ENDIF
C
C     Average new and previous values to accelerate convergence
 198      SIGITE =  0.5D0*(cg%SIGMA(JP1) + SIGITE)
          HITE = 0.5D0*(cg%H(JP1) + HITE)
          IF(cg%IANGLE.EQ.1) VITE = 0.5D0*(cg%VMEAN(JP1) + VITE)
          IF(cg%IROLL.EQ.1) RQITE = 0.5D0*(cg%RQ(JP1)+RQITE)
C
 200    CONTINUE
C
C*****End of iteration: DO 200 ITE = 1 to MAXITE*********************
C
C     The iteration did not converge
        if (cg%enable_cshore_outputfiles) then
        WRITE(40,2903) MAXITE, EPS1, JP1, L, cg%TIME, cg%QO(L)
        endif
C     Adopt the last iteration values
        cg%SIGMA(JP1) = SIGITE
        cg%H(JP1) = HITE
        IF(cg%IANGLE.EQ.1) cg%VMEAN(JP1) = VITE
        IF(cg%IROLL.EQ.1) cg%RQ(JP1) = RQITE
 2903   FORMAT(/'WARNING: Convergence was not reached after MAXITE= ',
     +     I4/ ' iterations with relative error EPS1 = ',E17.5/
     +     'at node JP1 = ',I4, ' Line L=',I3, ' TIME= ',F13.3/
     +     'QO(L)=',F13.9)
C
 210    cg%HRMS(JP1) = cg%SQR8*cg%SIGMA(JP1)
        cg%WSETUP(JP1) = cg%H(JP1) - cg%SWLDEP(JP1,L)
C
        IF(cg%IWCINT.EQ.1) THEN
          IF(cg%IANGLE.EQ.0) THEN
            QDISP = cg%QWX
          ELSE
            cg%QWY = cg%H(JP1)*cg%VMEAN(JP1) + cg%GRAV*
     +          cg%SIGMA(JP1)**2.D0*
     +       cg%STHETA(JP1)/cg%CP(JP1)
            IF(cg%IROLL.EQ.1) cg%QWY=cg%QWY + cg%RQ(JP1)*cg%STHETA(JP1)
            QDISP = cg%QWX*cg%CTHETA(JP1) + cg%QWY*cg%STHETA(JP1)
          ENDIF
        ENDIF
C
        CALL LWAVE(cg,JP1, L, cg%H(JP1), QDISP)
        CALL DBREAK(cg,JP1, L, cg%HRMS(JP1), cg%H(JP1))

c       lzhu added: energy dissipation rate due to veg is computed
        IF (cg%IVEG.EQ.3) THEN
           CALL DVEG  (cg,JP1, L, cg%HRMS(JP1), cg%H(JP1))
        ENDIF
c       lzhu edits end

        cg%SIGSTA(JP1) = cg%SIGMA(JP1)/cg%H(JP1)
        IF(cg%SIGSTA(JP1).GT.cg%SISMAX) cg%SIGSTA(JP1) = cg%SISMAX
        SIGT = cg%SIGSTA(JP1)*cg%CP(JP1)
        IF(cg%IANGLE.EQ.0) THEN
          VSIGT = 0.D0
        ELSE
          VSIGT = cg%VMEAN(JP1)/SIGT
        ENDIF
C
        cg%QWX=cg%QO(L)
        IF(cg%IPERM.EQ.1) THEN
          PKHSIG = cg%WKP*cg%H(JP1)*cg%SIGSTA(JP1)
          DEDX = (cg%WSETUP(JP1) - cg%WSETUP(J))/cg%DX
          CALL POFLOW(cg,JP1,L,PKHSIG,DEDX)
          cg%QWX = cg%QO(L) - cg%QP(JP1)
        ENDIF
        IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) cg%QWX=cg%QWX+QTIDE(JP1)
C
        SIGMA2 = cg%SIGMA(JP1)**2.D0
        cg%SXXSTA(JP1) = SIGMA2*cg%FSX
        IF(cg%IROLL.EQ.1) cg%SXXSTA(JP1)=cg%SXXSTA(JP1)+cg%RX(JP1)*
     +          cg%RQ(JP1)
        IF(cg%IWCINT.EQ.1) cg%SXXSTA(JP1)=cg%SXXSTA(JP1)+cg%QWX*
     +          cg%QWX/cg%GRAV/cg%H(JP1)
        cg%EFSTA(JP1) = SIGMA2*cg%FE
        DUM3 = cg%CP(JP1)*cg%CP(JP1)/cg%GRAV
        IF(cg%FB2(JP1,L).GT.0.D0) THEN
          USIGT = -cg%CTHETA(JP1)*cg%SIGSTA(JP1)*cg%H(JP1)/DUM3
          IF(cg%IROLL.EQ.1) THEN
            USIGT = USIGT*(1.D0+(cg%CP(JP1)/cg%GRAV)*cg%RQ(JP1)/SIGMA2)
          ENDIF
          IF(cg%IWCINT.EQ.1) USIGT=USIGT+cg%QWX/cg%H(JP1)/SIGT
          CALL GBXAGF(cg,cg%CTHETA(JP1),USIGT,cg%STHETA(JP1),VSIGT,
     +          cg%GBX(JP1),
     +       cg%GF(JP1))
          cg%TBXSTA(JP1)=cg%FB2(JP1,L)*cg%GBX(JP1)*SIGT**2.D0/cg%GRAV
          cg%DFSTA(JP1)=cg%FB2(JP1,L)*cg%GF(JP1)*SIGT**3.D0/cg%GRAV
c         lzhu change to 1=<cg%IVEG<=2
c         For cg%IVEG=3, cg%TBXSTA and cg%DFSTA are solely due to bottom friction
          IF(cg%IVEG.GE.1.AND.cg%IVEG.LE.2) THEN
            DUM=cg%VEGH(JP1,L)
            IF(DUM.GT.cg%H(JP1)) DUM=cg%H(JP1)
            VEGCV=1.0D0+DUM*cg%VEGFB(JP1,L)
            cg%TBXSTA(JP1)=VEGCV*cg%TBXSTA(JP1)
            cg%DFSTA(JP1)=VEGCV*cg%DFSTA(JP1)
          ENDIF
        ELSE
          cg%TBXSTA(JP1) = 0.D0
          cg%DFSTA(JP1) = 0.D0
        ENDIF
C
        IF(cg%IANGLE.EQ.1) THEN
          cg%SXYSTA(JP1) = cg%FSY*SIGMA2
          IF(cg%IROLL.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+cg%RY(JP1)*
     +          cg%RQ(JP1)
          IF(cg%IWCINT.EQ.1) cg%SXYSTA(JP1)=cg%SXYSTA(JP1)+cg%QWX*
     +          cg%QWY/cg%GRAV/cg%H(JP1)
          DUM2 = cg%SXYSTA(JP1) - cg%SXYSTA(J)
          SIGN=cg%STHETA(JP1)*DUM2
          IF(SIGN.GT.0.D0) DUM2=0.D0
          cg%TBYSTA(JP1) = -DUM2/cg%DX + cg%TWYSTA(ITIME)
          IF(cg%ITIDE.EQ.1) cg%TBYSTA(JP1)=cg%TBYSTA(JP1)-cg%H(JP1)*
     +          SMDEDY(JP1)
          IF(J.EQ.1) THEN
            cg%TBYSTA(J) = cg%TBYSTA(JP1)
            cg%VMEAN(J) = cg%VMEAN(JP1)
          ENDIF
          cg%GBY(JP1) = cg%TBYSTA(JP1)/cg%FB2(JP1,L)/DUM3/
     +          cg%SIGSTA(JP1)/cg%SIGSTA(JP1)
          IF(cg%IVEG.GE.1) cg%GBY(JP1)=cg%GBY(JP1)/VEGCV
        ENDIF
C
        JDUM = cg%JMAX(L)
        IF(cg%RCREST(L).GT.cg%SWLBC(ITIME)) JDUM=cg%JCREST(L)
C     If cg%IWTRAN=1 and cg%IOFLOW=1, overflow occurs on submerged crest
        IF(cg%IWTRAN.EQ.1.AND.cg%IOFLOW.EQ.1) JDUM=cg%JCREST(L)
        IF(cg%H(JP1).LT.EPS1.OR.JP1.EQ.JDUM) GOTO 400
C
        GOTO 100
C
C----------------End of LANDWARD MARCHING COMPUTATION -------------
C
 400    CONTINUE
C
        cg%JR = JP1
        cg%XR = cg%XB(cg%JR)
        cg%ZR = cg%ZB(cg%JR,L)
C
C BDJ 2011->2014 on 2014-10-02
          CALL SRFSP(cg,L)
C end BDJ 2011->2014 on 2014-10-02

C     If cg%IOVER=1, Subr.10 QORATE computes for cross-shore line L
C     cg%QO(L) = sum of wave overtopping, overflow and seepage rates
C     If cg%IOVER=0, cg%QO(L)=0.0, no iteration and no wet/dry zone
C     If cg%IWTRAN=1 and cg%JR=cg%JMAX(L), no wet and dry zone in computattion domain
C     Assume no water flux at landward end of computation domain
        IF(cg%IOVER.EQ.1) THEN
          IF(cg%IWTRAN.EQ.1.AND.cg%JR.EQ.cg%JMAX(L)) THEN
            cg%JWD=cg%JR
            cg%JDRY=cg%JR
            cg%QO(L)=0.D0
            GOTO 405
          ELSE
            ICONV = 1
            QOUSED = cg%QO(L)
            CALL QORATE(cg,ITIME,L,ITEQO,ICONV,0)
            IF(ICONV.EQ.0) GOTO 405
            IF(ICONV.EQ.1) then
              if (cg%enable_cshore_outputfiles) then
              WRITE(40,2904) cg%JR,L,cg%TIME,ITEQO,
     +          QOUSED,cg%QO(L)
              endif
            endif
          ENDIF
        ELSE
          cg%JWD = cg%JR
          cg%JDRY = cg%JR
          cg%QO(L)=0.D0
          GOTO 405
        ENDIF
 2904   FORMAT(/'NO CONVERGENCE OF cg%QO ITERATION'/
     +     'Landward end node cg%JR=', I6,' Line=',I3,
     +          ' cg%TIME=',F13.3/
     +     'Iteration number ITEQO=',I3,'   assumed cg%QO=',F13.9/
     +     'computed cg%QO=',F13.9)
C
        IF(ITEQO.LT.ITEMAX) GOTO 777
C
C....................END OF cg%QO ITERATION...........................
 405    CONTINUE
C
C     Calculate the standard deviation and mean of the horizontal
C     velocities U and V
C
        DO 410 I = 1,cg%JR
          SIGT = cg%CP(I)*cg%SIGSTA(I)
          cg%USTD(I) = SIGT*cg%CTHETA(I)
          cg%UMEAN(I)= -cg%USTD(I)*cg%SIGSTA(I)*cg%GRAV*cg%H(I)/
     +          cg%CP(I)/cg%CP(I)
          IF(cg%IROLL.EQ.1) cg%UMEAN(I)=cg%UMEAN(I)*(1.D0+(cg%CP(I)/
     +          cg%GRAV)*
     +       cg%RQ(I)/cg%SIGMA(I)**2.D0)
          cg%QWX = cg%QO(L)
          IF(cg%IPERM.EQ.1) cg%QWX=cg%QO(L)-cg%HP(I,L)*cg%UPMEAN(I)
          IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) cg%QWX=cg%QWX+QTIDE(I)
          cg%UMEAN(I) = cg%UMEAN(I) + cg%QWX/cg%H(I)
          IF(SIGT.GT.1.D-10) THEN !bdj
             cg%USTA(I)=cg%UMEAN(I)/SIGT
             cg%USTA(I) = min(cg%USTA(I),1.D0)
          ELSE
            cg%USTA(I)=0.D0
          ENDIF
          IF(cg%IANGLE.EQ.1) THEN
            cg%VSTD(I) = SIGT*DABS(cg%STHETA(I))
            cg%VSTA(I) = cg%VMEAN(I)/SIGT
          ENDIF
 410    CONTINUE
C
C     If cg%IOVER=1, connect cg%H(J) and cg%UMEAN(J) with J=1 to cg%JR with wet/dry-
C     zone cg%HWD(J) and cg%UMEAWD(J) with J=cg%JWD to cg%JDRY using Subr.17 TRANWD
C     also connect the corresponding standard deviations.
        IF(cg%IOVER.EQ.1) THEN
          cg%PWET(1:cg%JWD)=1.D0
          IF(cg%JDRY.GT.cg%JR) THEN
            CALL TRANWD(cg%H,cg%JR,cg%HWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%SIGMA,cg%JR,cg%SIGWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%UMEAN,cg%JR,cg%UMEAWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%USTD,cg%JR,cg%USTDWD,cg%JWD,cg%JDRY)
            IF(cg%IPERM.EQ.1) CALL TRANWD(cg%UPMEAN,cg%JR,cg%UPMWD,
     +          cg%JWD,cg%JDRY)
            IF(cg%IANGLE.EQ.1) THEN
              CALL TRANWD(cg%VMEAN,cg%JR,cg%VMEAWD,cg%JWD,cg%JDRY)
              CALL TRANWD(cg%VSTD,cg%JR,cg%VSTDWD,cg%JWD,cg%JDRY)
            ENDIF
          ELSE
            cg%JDRY=cg%JR
            IF(cg%JWD.LT.cg%JR) THEN
              JDUM=cg%JWD+1
              cg%PWET(JDUM:cg%JR)=1.D0
            ENDIF
          ENDIF
        ENDIF
C     Smooth computed cg%H(J), cg%SIGMA(J), cg%USTD(J), cg%UMEAN(J), cg%USTA(J), cg%DFSTA(J),
C     cg%DBSTA(J),cg%RQ(J), cg%VMEAN(J), cg%VSTD(J) and cg%VSTA(J) using Subr. 14 SMOOTH
        DUMVEC = cg%H
        CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%H)
        DUMVEC = cg%SIGMA
        CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%SIGMA)
        DUMVEC = cg%USTD
        CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%USTD)
        DUMVEC = cg%UMEAN
        CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%UMEAN)
        DUMVEC = cg%USTA
        CALL SMOOTH(cg,cg%JR,DUMVEC,cg%USTA)
        DUMVEC = cg%DFSTA
        CALL SMOOTH(cg,cg%JR,DUMVEC,cg%DFSTA)
        IF(cg%IPERM.EQ.1) THEN
          DUMVEC=cg%UPMEAN
          CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%UPMEAN)
          IF(cg%IOVER.EQ.1) THEN
            DO 420 J=2,cg%JDRY
              DUM=cg%ZP(J,L)
              IF(DUM.LT.cg%SWLBC(ITIME).AND.cg%ZP(J,L).GE.cg%ZP(J-1,L))
     +           DUM=cg%SWLBC(ITIME)
              ETAPOR=cg%ZB(J,L)*cg%PWET(J)+DUM*(1.D0-cg%PWET(J))
              cg%QP(J)=cg%UPMEAN(J)*(ETAPOR-cg%ZP(J,L))*cg%PWET(J)
 420        CONTINUE
          ENDIF
        ENDIF
        IF(cg%IROLL.EQ.0) THEN
          DUMVEC=cg%DBSTA
          CALL SMOOTH(cg,cg%JR,DUMVEC,cg%DBSTA)
        ELSE
          DUMVEC = cg%RQ
          CALL SMOOTH(cg,cg%JR,DUMVEC,cg%RQ)
        ENDIF
        IF(cg%IANGLE.EQ.1) THEN
          DUMVEC = cg%VMEAN
          CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%VMEAN)
          DUMVEC = cg%VSTD
          CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%VSTD)
          DUMVEC = cg%VSTA
          CALL SMOOTH(cg,cg%JR,DUMVEC,cg%VSTA)
        ENDIF
C
C     Subr. 21 WTRANS computes transmitted waves (cg%IWTRAN=1) landward of
C     an emerged structrue or barrier island if entire structure is not
C     submerged and standing water exists
        IF(cg%IWTRAN.EQ.1.AND.cg%JR.LT.cg%JMAX(L)) THEN
          ICHECK=0
          IF(cg%JSWL(L).EQ.cg%JMAX(L).AND.cg%IOFLOW.EQ.0) ICHECK=1
          JEND=cg%JSL1
          IF(ICHECK.EQ.1) JEND=cg%JMAX(L)
          IF(cg%JDRY.LT.JEND) THEN
            JDUM=cg%JDRY+1
            DO 425 J=JDUM,JEND
              cg%PWET(J)=0.D0
              cg%H(J)=0.D0
              IF(cg%ISWLSL.LE.1) THEN
                IF(cg%IOFLOW.EQ.0.OR.cg%JSL.LT.cg%JMAX(L)) THEN
                  IF (cg%SWLDEP(J,L).GT.0.D0) THEN
                    cg%PWET(J)=1.D0
                    cg%H(J)=cg%SWLDEP(J,L)
                  ENDIF
                ENDIF
              ENDIF
              cg%SIGMA(J)=0.D0
              cg%WSETUP(J)=0.D0
              cg%SIGSTA(J)=0.D0
              cg%UMEAN(J)=0.D0
              cg%USTD(J)=0.D0
              IF(cg%IPERM.EQ.1) THEN
                cg%QP(J)=cg%QO(L)
                IF(ICHECK.EQ.1) cg%QP(J)=0.D0
                IF(cg%HP(J,L).GT.1.D-3) THEN
                  cg%UPMEAN(J)=cg%QP(J)/cg%HP(J,L)
                ELSE
                  cg%UPMEAN(J)=0.D0
                ENDIF
              ENDIF
              IF(cg%IANGLE.EQ.1) THEN
                cg%VMEAN(J)=0.D0
                cg%VSTD(J)=0.D0
                cg%STHETA(J)=0.D0
              ENDIF
 425        CONTINUE
          ENDIF
C     IF cg%IWTRAN=1 and cg%JDRY is less than cg%JSL1=(cg%JSL-1), no water at
C     nodes between cg%JDRY and cg%JSL but water flux cg%QP(J) in permeable
C     layer is assumed constant
          IF(ICHECK.EQ.0.AND.cg%JSL.LT.cg%JMAX(L)) CALL
     +          WTRANS(cg,ITIME,L)
        ENDIF
C
C     Subr.11 SEDTRA computes the cross-shore and longshore
C     sediment transport rates if cg%IPROFL = 1
C     If a vertical wall exists, cg%IVWALL=2 indicates exposure to
C     wave action along cross-shore line L
        IF(cg%IPROFL.EQ.1) THEN
            IF(cg%IVWALL(L).GE.1) THEN
              IF(cg%ZB(cg%JMAX(L)-1,L).LT.cg%ZP(cg%JMAX(L),L)) THEN
                cg%IVWALL(L)=2
              ELSE
                cg%IVWALL(L)=1
              ENDIF
            ENDIF
          CALL SEDTRA(cg,L)
        ENDIF
C
C     If cg%IPROFL=1, Subr.12 CHANGE computes the change of the
C     bottom elevation, cg%DELZB(j), at node j during the cg%TIME step
C     cg%DELT determined in this subroutine which also checks whether
C     IEND=1 and the end of given ITIME is reached.
C     cg%VY(J,L)=total longshore sediment transport rate integrated from
C     cg%TIMEBC(ITIME) to cg%TIMEBC(ITIME+1) used in Subr.12 CHANGE if cg%IQYDY=1
C     If cg%ISTSAN=1,cg%ZB(J,L) and cg%ZP(J,L) at next cg%TIME level are computed in Subr. 12 CHANGE
C     If cg%ICLAY=1,Subr. 22 EROSON is called to compute erosion of clay below sand
C
        IF(cg%IPROFL.EQ.1) THEN
          CALL CHANGE(cg,ITIME,L,IEND,1)
        DO 430 J=1,cg%JMAX(L)
          IF(cg%TIME.EQ.cg%TIMEBC(ITIME)) THEN
            cg%VY(J,L)=0.D0
            cg%DZX(J,L)=cg%ZB(J,L)
          ENDIF
          IF(cg%ISTSAN.EQ.0) cg%ZB(J,L)=cg%ZB(J,L)+cg%DELZB(J,L)
          IF(cg%ICLAY.EQ.1) CALL EROSON(cg,ITIME,L,IEND)
          IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
            cg%HP(J,L)=cg%ZB(J,L)-cg%ZP(J,L)
            IF(cg%HP(J,L).LT.0.D0) THEN
              cg%HP(J,L)=0.D0
              cg%ZB(J,L)=cg%ZP(J,L)
            ENDIF
            IF(cg%ISEDAV.EQ.2) THEN
              IF(cg%ZB(J,L).LT.cg%ZMESH(J,L)) cg%ZB(J,L)=cg%ZMESH(J,L)
            ENDIF
          ENDIF
          IF(cg%TIME.EQ.0.D0) THEN
            cg%VBX(J,L)=0.D0
            cg%VSX(J,L)=0.D0
            cg%VBY(J,L)=0.D0
            cg%VSY(J,L)=0.D0
          ENDIF
          cg%VBX(J,L)=cg%VBX(J,L)+cg%DELT*cg%QBX(J)
          cg%VSX(J,L)=cg%VSX(J,L)+cg%DELT*cg%QSX(J)
          IF(cg%IANGLE.EQ.1) THEN
            cg%VBY(J,L)=cg%VBY(J,L)+cg%DELT*cg%QBY(J)
            cg%VSY(J,L)=cg%VSY(J,L)+cg%DELT*cg%QSY(J)
            cg%VY(J,L)=cg%VY(J,L)+cg%DELT*(cg%QBY(J)+cg%QSY(J))
          ENDIF
 430    CONTINUE
          IF(cg%IVWALL(L).GE.1) THEN
            IF(cg%ZB(cg%JMAX(L),L).LT.cg%ZP(cg%JMAX(L),L))
     +          cg%ZB(cg%JMAX(L),L)=cg%ZP(cg%JMAX(L),L)
          ENDIF
        ENDIF
        IF(cg%IPROFL.EQ.0) THEN
          IEND=1
          cg%DELT = 1.D0
        ENDIF
        IF(cg%IQYDY.EQ.1.AND.IEND.EQ.1) THEN
          IF(L.EQ.cg%ILINE) CALL CHANGE(cg,ITIME,L,IEND,2)
        ENDIF
C
C     If cg%IVEG=1 and cg%IPROFL=1, compute the vegetation height
C     cg%VEGH(J,L) above the local bottom cg%ZB(J,L) in vegetated zone
C     with cg%UPROOT(J,L)=1.0. Check whether the vegetation is buried
C     or uprooted using the fixed upper and lower elevations of the
C     vegetation denoted as cg%VEGZD(J,L) and cg%VEGZR(J,L)
C
c     lzhu change to (cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3)
      IF((cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3).AND.cg%IPROFL.EQ.1) THEN
        DO 440 J=1,cg%JMAX(L)
          IF(cg%UPROOT(J,L).EQ.1.D0) THEN
            cg%VEGH(J,L)=cg%VEGZD(J,L)-cg%ZB(J,L)
            IF(cg%VEGH(J,L).LT.0.D0) cg%VEGH(J,L)=0.D0
            IF(cg%VEGZR(J,L).GE.cg%ZB(J,L)) THEN
              cg%UPROOT(J,L)=0.D0
              cg%VEGH(J,L)=0.D0
            ENDIF
          ENDIF
 440    CONTINUE
      ENDIF
C     where for buried vegetation, cg%VEGH(J,L)=0.0 and cg%UPROOT(J,L)=1.0,
C     whereas for uprooted vegetation, cg%VEGH(J,L)=0.0 and cg%UPROOT(J,L)=0.0
C
C     If cg%IPROFL=2, Subr.22 EROSON computes erosion of grassed dike
C     and resulting bottom elevation cg%ZB(J,L) where this subroutine
C     determines cg%TIME step cg%DELT and whether the end of given ITIME
C     is reached
      IF(cg%IPROFL.EQ.2) CALL EROSON(cg,ITIME,L,IEND)
C
C     If cg%IOVER=1, store cg%TIME series of wave overtopping rate and
C     sediment transport rates at landward end node cg%JMAX
C     cg%QO(L)=sum of wave overtopping, overflow and seepage rates
C
        IF(cg%IOVER.EQ.1) THEN
          IF(cg%TIME.EQ.cg%TIMEBC(ITIME)) THEN
            cg%TSQO(L) = 0.D0
            cg%TSQBX(L) = 0.D0
            cg%TSQSX(L) = 0.D0
          ENDIF
          IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
            cg%TSQO(L)=cg%TSQO(L)+cg%DELT*cg%QM
          ELSE
            cg%TSQO(L)=cg%TSQO(L)+cg%DELT*cg%QO(L)
          ENDIF
          cg%TSQBX(L)=cg%TSQBX(L)+cg%DELT*cg%QBX(cg%JMAX(L))
          cg%TSQSX(L)=cg%TSQSX(L)+cg%DELT*cg%QSX(cg%JMAX(L))
        ENDIF
C
C     Subr.8 OUTPUT stores computed results when IEND=1
C     Put "c" below if no output when cg%TIME = 0
C       IF(cg%TIME.EQ.0.D0) CALL OUTPUT(ITIME,L,ITEQO,ICONV)
        IF(cg%IPROFL.GE.1) cg%TIME=cg%TIME+cg%DELT

        if (cg%enable_varscheck_alltime) then
          write (varscheck_label, *) cg%TIME
          dump_vars_status = dump_vars(cg, varscheck_label)
        endif

        IF(IEND.EQ.1) THEN
          if (cg%enable_cshore_outputfiles) then
          CALL OUTPUT(cg,ITIME,L,ITEQO,ICONV)
          endif
        ENDIF
        IF(cg%IPROFL.EQ.0) cg%TIME=cg%TIMEBC(ITIME+1)
C
C     Compute the cg%BSLOPE at cg%TIME = (cg%TIME+cg%DELT)
        IF(cg%IPROFL.GE.1) THEN
          DO 501 J=1,cg%JMAX(L)
            IF(J.EQ.1) THEN
              cg%BSLOPE(1,L)=(cg%ZB(2,L)-cg%ZB(1,L))/cg%DX
            ELSE
              IF(J.EQ.cg%JMAX(L)) THEN
                cg%BSLOPE(cg%JMAX(L),L)=(cg%ZB(cg%JMAX(L),L)-
     +          cg%ZB(cg%JMAX(L)-1,L))/cg%DX
              ELSE
                cg%BSLOPE(J,L)=(cg%ZB(J+1,L)-cg%ZB(J-1,L))/cg%DX2
              ENDIF
            ENDIF
 501      CONTINUE
C     Compute new bottom cg%RCREST if cg%IPOND=0
          IF(cg%IPOND.EQ.0) THEN
            cg%RCREST(L) = cg%ZB(1,L)
            DO 502 J=2,cg%JMAX(L)
              DUM = cg%ZB(J,L) - cg%RCREST(L)
              IF(DUM.GE.0.D0) THEN
                cg%RCREST(L) = cg%ZB(J,L)
                cg%JCREST(L) = J
              ENDIF
 502        CONTINUE
          ENDIF
        ENDIF
C
C     If IEND=0, go to 888 for the next landward marching computation
        IF(IEND.EQ.0) GOTO 888
C
C     If IEND=1 and L is less than cg%ILINE, reset cg%TIME for next cross-shore line
        IF(L.LT.cg%ILINE) THEN
          cg%TIME=cg%TIMEBC(ITIME)
        ENDIF
C
C     IEND=1 and specify the seaward input for the next value of
C     ITIME if ITIME is less than cg%NTIME and L=cg%ILINE.
        IF(ITIME.LT.cg%NTIME.AND.L.EQ.cg%ILINE) THEN
          ITIME1=ITIME+1
          cg%TP=cg%TPBC(ITIME1)
          cg%HRMS(1)=cg%HRMSBC(ITIME1)
c         lzhu added
        IF (cg%IVEG.EQ.3.AND.cg%IDISS.GE.2) THEN
            cg%FREQMIN=cg%FREQMINBC(ITIME1)
            cg%FREQMAX=cg%FREQMAXBC(ITIME1)
            cg%FREQNUM=cg%FREQNUMBC(ITIME1)
        ENDIF
C     cg%NPT=integer used in Subr.14 SMOOTH
C     cg%NPE=integer used in Subr.15 EXTRAPO
          cg%NPT=1+NINT(cg%HRMS(1)/cg%DX)
          cg%NPE=1+NINT(cg%HRMS(1)/cg%DX2)
          IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.1) THEN
C           cg%NPT=cg%NPT+NINT(cg%HRMS(1)/cg%DXD2)
            IF(cg%HRMS(1).LT.0.05) cg%NPT=cg%NPT+NINT(cg%HRMS(1)/cg%DX)
          ENDIF
C         IF(cg%IVWALL(L).EQ.2) cg%NPT=cg%NPT+NINT(cg%HRMS(1)/cg%DX)
          cg%WSETUP(1)=cg%WSETBC(ITIME1)
          cg%ANGLE=cg%WANGBC(ITIME1)
C     No wave and current interaction for cg%IANGLE=1
          cg%WKPO=cg%TWOPI*cg%TWOPI/(cg%GRAV*cg%TP*cg%TP)
C     where cg%WKPO is the deep water wave number
        ENDIF
C
 998  CONTINUE

      if (cg%enable_varscheck_t0tend .or.
     +  cg%enable_varscheck_alltime) then
        dump_vars_status = dump_vars(cg, "tend")
      endif

      cg%MAXMWL(1) = (cg%H(cg%JDRY)+cg%ZB(cg%JDRY,1))

c      print *, 'cshore jdry, setup',cg%JDRY,
c     + (cg%H(cg%JDRY)+cg%ZB(cg%JDRY,1)),

C     **************** END OF cg%ILINE COMPUTATIAON ***************************
C
c 999  CONTINUE
C
C     **************** END OF cg%TIME MARCHING COMPUTATION ********************
      END SUBROUTINE compute
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
C     #01####################  SUBROUTINE OPENER  ########################
C
C     This subroutine opens all input and output files
C
      SUBROUTINE OPENER(cg, BASENAME)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      character (len=*), intent (in) :: BASENAME

      ! open input file
        OPEN(UNIT=11,FILE=trim(BASENAME)//'infile',
     +  STATUS='OLD',ACCESS='SEQUENTIAL')

      if (cg%enable_cshore_outputfiles) then
      ! open output files
        OPEN(UNIT=20,FILE=trim(BASENAME)//'ODOC',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=21,FILE=trim(BASENAME)//'OBPROF',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=22,FILE=trim(BASENAME)//'OSETUP',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=23,FILE=trim(BASENAME)//'OPARAM',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=24,FILE=trim(BASENAME)//'OXMOME',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=25,FILE=trim(BASENAME)//'OYMOME',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=26,FILE=trim(BASENAME)//'OENERG',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=27,FILE=trim(BASENAME)//'OXVELO',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=28,FILE=trim(BASENAME)//'OYVELO',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=29,FILE=trim(BASENAME)//'OROLLE',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=30,FILE=trim(BASENAME)//'OBSUSL',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=31,FILE=trim(BASENAME)//'OPORUS',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=32,FILE=trim(BASENAME)//'OCROSS',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=33,FILE=trim(BASENAME)//'OLONGS',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=34,FILE=trim(BASENAME)//'OSWASH',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=35,FILE=trim(BASENAME)//'OSWASE',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=36,FILE=trim(BASENAME)//'OTIMSE',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=37,FILE=trim(BASENAME)//'OCRVOL',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=38,FILE=trim(BASENAME)//'OLOVOL',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=39,FILE=trim(BASENAME)//'ODIKER',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      OPEN(UNIT=40,FILE=trim(BASENAME)//'OMESSG',
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
      endif

      RETURN
      END SUBROUTINE
C
C     -01-----------------  END OF SUBROUTINE OPENER  --------------------


C     #02#####################  SUBROUTINE INPUT  #######################
C
C     This subroutine reads data from primary input data file
C
      SUBROUTINE INPUT(cg,VER)
C
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000, NB=30000, NL=100,NSPECTRUM=5000)
      CHARACTER COMMEN*70, VER*90 !bdj
      DIMENSION TWAVE(NB),TPIN(NB),HRMSIN(NB),WANGIN(NB),TSURG(NB),
     +     SWLIN(NB),TWIND(NB),WIND10(NB),WINDAN(NB),TSLAND(NB),
     +     SLANIN(NB),TTIDE(NB),DEDYIN(NB),DSDTIN(NB),
     +     FREQMININ(NB),FREQMAXIN(NB),FREQNUMIN(NB)
      logical :: printstatus
      integer :: io_stat
C
C     Gravitational acceleration cg%GRAV = 9.81 (m/s/s)
      cg%GRAV=9.81D0
C
C.............. COMMENT LINES
C     Write the first line of this CSHORE program on ODOC output file
      if (cg%enable_cshore_outputfiles) WRITE (20,*) VER !bdj
      if (cg%enable_cshore_stdout) WRITE (*,*) VER !bdj
C     NLINES = number of comment lines preceding input data
C     READ (11,1110) NLINES
      READ (11,*) DUM !bdj
      NLINES=NINT(DUM) !bdj
C
      DO 110 I = 1,NLINES
C       READ  (11,1120) (COMMEN(J),J=1,14)
C       WRITE (20,1120) (COMMEN(J),J=1,14)
C       WRITE (*,*) (COMMEN(J),J=1,14)
        READ (11,'(A70)') COMMEN !bdj
        if (cg%enable_cshore_outputfiles) WRITE (20,*) COMMEN !bdj
        if (cg%enable_cshore_stdout) WRITE (*,*) COMMEN !bdj
 110  CONTINUE
C
C.............. INPUT COMPUTATION OPTIONS
C     cg%ILINE = number of cross-shore lines
C     cg%IQYDY=0 to neglect alongshore gradient of longshore sediment transport
C     cg%IQYDY=1 to include alongshore gradient for profile evolution
C
C     READ(11,1110) cg%ILINE
      READ(11,*) DUM      !bdj
      cg%ILINE = NINT(DUM)   !bdj
      cg%IQYDY=0
C     IF(cg%ILINE.GT.2) READ(11,1110) cg%IQYDY
      IF(cg%ILINE.GT.2) THEN
        READ(11,*) DUM  !bdj
        cg%IQYDY=NINT(DUM) !bdj
      ENDIF
      IF(cg%ILINE.GT.NL) THEN
        WRITE(*,*) '**Error** ILINE is lager than NL'
        STOP
      ENDIF
C
C     cg%IPROFL=0 for fixed bottom profile(no input for cg%ISEDAV=0)
C     cg%IPROFL=1 for profile evolution computation(input cg%ISEDAV)
C     cg%IPROFL=1.1 for cg%IPROFL=1 and no initial bottom smoothing
C     cg%IPROFL=2 for dike erosion computation (cg%ISEDAV=0)
C     cg%ISEDAV=0 for unlimited bottom sediment
C     cg%ISEDAV=1 for sediment availability limited by hard bottom
C     cg%ISEDAV=2 for stone availability limited by wire mesh
C     For cg%IPROFL=0 and 2, cg%IQYDY=0 is imposed
C
C     READ(11,1110) cg%IPROFL
      READ(11,*) DUM      !bdj
c     bdj 2018-08-28 to input cg%ISMOOTH
      tmp = nint(10.*(dum-nint(dum)))
      cg%ISMOOTH=1
      if(tmp.eq.1) then
         cg%ISMOOTH=0
      endif
c     end bdj 2018-08-28 to input cg%ISMOOTH
      cg%IPROFL = NINT(DUM)   !bdj
      IF(cg%IPROFL.EQ.0.OR.cg%IPROFL.EQ.2) cg%IQYDY=0
      cg%ISEDAV=0
C     IF(cg%IPROFL.EQ.1) READ(11,1110) cg%ISEDAV
      IF(cg%IPROFL.EQ.1) THEN
        READ(11,*) DUM     !bdj
        cg%ISEDAV = NINT(DUM) !bdj
      ENDIF
C
C.............. INPUT COMPUTATION OPTION
C     cg%IPERM=0 for impermeable bottom
C     cg%IPERM=1 for permeable bottom of stone structure
C
C     READ(11,1110) cg%IPERM
      READ(11,*) DUM    !bdj
      cg%IPERM = NINT(DUM) !bdj
      IF(cg%ISEDAV.EQ.2.AND.cg%IPERM.EQ.0) THEN
        WRITE(*,*) '**Error** For ISEDAV=2, IPERM=1 is required'
        STOP
      ENDIF
C
C...........INPUT COMPUTATION OPTIONS
C     cg%IOVER=0 for no wave overtopping and overflow(no additional input)
C     cg%IOVER=1 for wave overtopping and overflow on crest(input cg%IWTRAN)
C     where for dike erosion (cg%IPROFL=2), cg%IOVER=1 to predict erosion of
C     landward dike slope
C     cg%IWTRAN=0 for no standing water landward of crest above seaward SWL but
C     lanward SWL=seaward SWL if crest is submerged and no overflow (cg%IOFLOW=0)
C     cg%IWTRAN=1 for wave transmission into landward water level (input) and
C     if crest is below seaward SWL, wave overtopping and overflow on crest
C     (cg%IOFLOW=1)
C     cg%IPOND=0 for no ponding water landward of cg%JSWL node
C     cg%IPOND=1 for ponding water in runnel landward of ridge for cg%IPERM=0
C     cg%INFILT=0 for no infiltration landward of sand dune
C     cg%INFILT=1 for infiltration if cg%IOVER=1, cg%IPERM=0, cg%IPROFL=1
C
C     READ(11,1110) cg%IOVER
      READ(11, *) DUM   !bdj
      cg%IOVER = NINT(DUM) !bdj
      IF(cg%IPROFL.EQ.2) cg%IOVER=1
      cg%IWTRAN=0
      cg%IOFLOW=0
      cg%IPOND=0
      cg%INFILT=0
C     IF(cg%IOVER.EQ.1) READ(11,1110) cg%IWTRAN
      IF(cg%IOVER.EQ.1) READ(11,*)DUM;cg%IWTRAN=NINT(DUM) !bdj
C     IF(cg%IOVER.EQ.1.AND.cg%IWTRAN.EQ.0) READ(11,1110) cg%IPOND
      IF(cg%IOVER.EQ.1.AND.cg%IWTRAN.EQ.0) THEN
        READ(11,*)DUM   !bdj
        cg%IPOND=NINT(DUM) !bdj
      ENDIF
      IF(cg%IOVER.EQ.1.AND.cg%IPERM.EQ.0) THEN
C       IF(cg%IPROFL.EQ.1) READ(11,1110) cg%INFILT
        IF(cg%IPROFL.EQ.1) READ(11,*) DUM;cg%INFILT =NINT(DUM) !bdj
      ENDIF
C
C........... INPUT COMPUTATION OPTION
C     cg%IWCINT=0 for no wave and current interaction
C     cg%IWCINT=1 for wave and current interaction in frequency
C     dispersion, momentum and wave action equations
C
C     READ(11,1110) cg%IWCINT
      READ(11,*) DUM     !bdj
      cg%IWCINT = NINT(DUM) !bdj
C
C........... INPUT COMPUTATION OPTION
C     cg%IROLL=0 for no roller
C     cg%IROLL=1 for roller effects in governing equations
C
C     READ(11,1110) cg%IROLL
      READ(11,*) DUM    !bdj
      cg%IROLL = NINT(DUM) !bdj
      IF(cg%IROLL.EQ.1) cg%RBZERO=0.1D0
C
C........... INPUT COMPUTATION OPTION
C     cg%IWIND=0 for no wind effect
C     cg%IWIND=1 for wind shear stresses on momentum equations
C
C     READ(11,1110) cg%IWIND
      READ(11,*) DUM    !bdj
      cg%IWIND = NINT(DUM) !bdj
C
C........... INPUT COMPUTATION OPTION
C     cg%ITIDE=0 for no tidal effect on currents
C     cg%ITIDE=1 for longshore and cross-shore tidal currents
C
C     READ(11,1110) cg%ITIDE
      READ(11,*) DUM   !bdj
      cg%ITIDE = NINT(DUM)!bdj
C
C........... INPUT COMPUTATION OPTION
C     cg%IVEG=0 for no vegetation or vegetation represented by increased
C     bottom friction factor cg%FBINP
C     cg%IVEG=1 for vegataion whose density, width, height and root depth
C     are specified as input. The height and root depth vary with the
C     bottom elevation change
C     cg%IVEG=2 for vegatation whose constant density, width and height
C     are specified as input
c     lzhu added:
C     cg%IVEG=3 for vegetation whose density, width, height and root depth
c     are specified as input. You can select energy dissipatio model
c     and phase-averaged depth-integrated models
c     lzhu comments end
c
C     READ(11,1110) cg%IVEG
      READ(11,*) DUM
      cg%IVEG = NINT(DUM)

C........... INPUT COMPUTATION OPTION
c     lzhu: only when cg%IVEG=3, cg%IDISS and cg%IFV would be written to makeinfile and
c           read here in CSHORE source code
      IF (cg%IVEG.EQ.3) THEN
         READ(11,*) DUM
         cg%IDISS = NINT(DUM)

         READ(11,*) DUM
         cg%IFV = NINT(DUM)
      ENDIF

C........... INPUT COMPUTATION OPTION
C     cg%ISTSAN=0 except for fixed stone structure on sand bottom
C     cg%ISTSAN=1 for stone structure (cg%IPERM=1) on deforming bottom
C     (cg%IPROFL=1) of unlimited sand (cg%ISEDAV=0)
C     cg%CPSTON=empirical parameter for sand transport reduction on porous stone structure
      cg%ISTSAN=0
      IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.1) THEN
          IF(cg%ISEDAV.EQ.0) THEN
              cg%ISTSAN=1
              cg%CPSTON=1.0D0
          ENDIF
      ENDIF
C
C........... INPUT COMPUTATION OPTION
C     cg%ICLAY=0 except for eroding sand layer on erodible clay
C     cg%ICLAY=1 for sand layer (cg%ISEDAV=1 and cg%IPERM=0) above clay
C     bottom (eroded by wave action) with no vegetation (cg%IVEG=0)
      cg%ICLAY=0
      IF(cg%ISEDAV.EQ.1.AND.cg%IPERM.EQ.0) THEN
          IF(cg%IVEG.EQ.0) THEN
              READ(11,*)DUM
              cg%ICLAY=NINT(DUM)
          ENDIF
      ENDIF
C
C........... COMPUTATIONAL INPUT DATA
C     cg%DX=nodal spacing for input bottom geometry
C
C     READ(11,1130) cg%DX
      READ(11,*) cg%DX !bdj
C
C........... BREAKER RATIO PARAMETER cg%GAMMA=0.5-1.0
C     READ(11,1130) cg%GAMMA
      READ(11,*) cg%GAMMA !bdj
C
c........... lzhu added on April 29, 2018 .........
c This parameter cg%DIKETOE is added to specify the location of the toe of the dike.
c After the location is read in, the corresponding KC value is computed for the
c purpose of finding out the kappa and runup.
c      READ(11,*) DUM
c      cg%IWEIBULL = NINT(DUM)
c      IF (cg%IWEIBULL.EQ.1) READ(11,*) cg%DIKETOE !lzhu

c........... end of lzhu changes on April 29, 2018 .........
C........... SEDIMENT CHARACTERISTICS IF cg%IPROFL=1
C     cg%WF    = sediment fall velocity (m/s)
C     cg%SG    = sediment specific gravity
C     cg%D50   = median sediment diameter (mm)
C     converted to (m) below
C     cg%EFFB  = suspension efficiency due to breaking, eB
C     cg%EFFF  = suspension efficiency due to friction, ef
C     cg%SLP   = suspended load parameter
C     cg%SLPOT = suspended load parameter due to wave overtopping if cg%IOVER=1
C     SPORO = sediment porosity (SPORO=0.4 for sand but input cg%SNP used for cg%IPERM=1)
C     cg%SHIELD= critical Shields parameter used if cg%D50 is less than cg%CSEDIA
C     cg%CSTABN= critical stability number (0.6 to 1.1) used if cg%IPERM=1
C     cg%CSEDIA= critical sediment diameter to separate sand and stone
C     cg%TANPHI= tangent (sediment friction cg%ANGLE)
C     cg%BLP   = bedload parameter
C     cg%BLD	= cg%BLP/cg%GRAV/(cg%SG-1) used for bedload transport rate
C     cg%BEDLM = parameter m for bedload reduction factor BRF for hard
C     bottom used for cg%ISEDAV=1
C     Following default values are specified to reduce input error
      IF(cg%IPROFL.EQ.1) THEN
        SPORO = 0.4D0
        cg%SHIELD = 0.05D0
C mg     cg%EFFF = 0.01D0
C        cg%EFFB = 0.002D0 to 0.01D0
C        cg%SLP = 0.1D0 to 0.4D0
C        cg%SLPOT = 0.1D0 to 3.6D0
c        cg%TANPHI = 0.63D0 for sand
c        cg%BLP = 0.001D0 to 0.004D0
        IF(cg%ISEDAV.GE.1) cg%BEDLM=1.0D0
C
C       READ (11,1150) cg%D50,cg%WF,cg%SG
        READ (11,*) cg%D50,cg%WF,cg%SG !bdj
C mg - added read for cg%EFFF
C       IF(cg%IOVER.EQ.0) READ (11,1150) cg%EFFB,cg%EFFF,cg%SLP
        IF(cg%IOVER.EQ.0) READ (11,*) cg%EFFB,cg%EFFF,cg%SLP !bdj
C       IF(cg%IOVER.EQ.1) READ (11,1150) cg%EFFB,cg%EFFF,cg%SLP,cg%SLPOT
        IF(cg%IOVER.EQ.1) READ (11,*) cg%EFFB,cg%EFFF,cg%SLP,cg%SLPOT !bdj
C mg
C       READ (11,1150) cg%TANPHI,cg%BLP
        READ (11,*) cg%TANPHI,cg%BLP !bdj
C mg
        IF(cg%EFFF.LT.cg%EFFB) THEN
          WRITE(*,*) ' ** Error ** The suspension efficiency parameter'
          WRITE(*,*) ' due to bottom friction must be greater than or '
          WRITE(*,*) ' equal to the suspension efficiency parameter due'
          WRITE(*,*) ' to wave breaking.'
          STOP
        ENDIF
C mg
        cg%D50    = cg%D50*1.D-3
        SGM1   = cg%SG - 1.D0
        cg%SPORO1 = 1.D0 - SPORO
        cg%WFSGM1 = cg%WF*SGM1
        cg%GSGM1  = cg%GRAV*SGM1
        IF(cg%IPERM.EQ.0.OR.cg%ISTSAN.EQ.1) THEN
          cg%GSD50S = cg%GSGM1*cg%D50*cg%SHIELD
          cg%CSEDIA=2.D0*cg%D50
        ENDIF
        cg%BLD = cg%BLP/cg%GSGM1
      ENDIF
C
C.....RUNUP WIRE HEIGHT cg%RWH (in meters) IF cg%IOVER=1
C     IF(cg%IOVER.EQ.1) READ (11,1130) cg%RWH
      IF(cg%IOVER.EQ.1) READ (11,*) cg%RWH !bdj
C
C.....STONE OR GRAVEL CHARACTERISTICS IF cg%IPERM=1
C     cg%SNP = Stone/gravel porosity in porous layer (cg%SNP can be different from sand
C     porosity=0.4 for cg%ISTSAN=1)
C     cg%SDP = Nominal stone/gravel diameter (m)
C     cg%CSTABN = Critical stability number (0.6 to 1.1) for stone
C
      IF(cg%IPERM.EQ.1) THEN
C       READ(11,1150) cg%SNP,cg%SDP,cg%CSTABN
        READ(11,*) cg%SNP,cg%SDP,cg%CSTABN
        IF(cg%IPROFL.EQ.1.AND.cg%ISTSAN.EQ.0) THEN
          cg%GSD50S = DSQRT(cg%GSGM1*cg%D50*cg%CSTABN)
          cg%CSEDIA=0.5D0*cg%D50
          SPORO=cg%SNP
          cg%SPORO1=1.D0-SPORO
        ENDIF
      ENDIF
C
C.....DIKE EROSION EFFICIENCIES IF cg%IPROFL=2
C     cg%DEEB=eB due to breaking waves
C     cg%DEEF=ef due to bottom friction
      IF(cg%IPROFL.EQ.2.OR.cg%ICLAY.EQ.1) THEN
        READ(11,*) cg%DEEB,cg%DEEF
      ENDIF
C
C     cg%HWDMIN=minimum water depth (m)
C     used in the wet and dry zone in Subr.16 WETDRY
C     cg%D50=median sediment diameter (m)
      IF(cg%IOVER.EQ.1) THEN
        cg%HWDMIN=1.D-6
        IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.0) THEN
          cg%HWDMIN=cg%D50
C         cg%HWDMIN=0.2D0*cg%HWDMIN
          IF(cg%DX.GE.0.05D0) cg%HWDMIN=1.D-3
        ELSE
C         cg%HWDMIN=1.D-5
          IF(cg%IPROFL.GE.1) cg%HWDMIN=1.D-4
        ENDIF
      ENDIF
C
C     ......... INPUT WAVE AND WATER LEVEL
C     cg%NWAVE      = number of waves at x=0 starting from cg%TIME=0
C     cg%NSURG      = number of water levels at x=0 from cg%TIME=0
C     cg%NTIME      = number of waves and water levels at x=0
C     During cg%TIME= cg%TIMEBC(i) to cg%TIME=cg%TIMEBC(i+1) if cg%NWAVE=cg%NSURG
C     cg%TIMEBC(i)  = cg%TIME in seconds at the beginning of the
C     specified wave and water level
C     cg%TPBC(i)    = spectral peak or wave period in seconds
C     cg%HRMSBC(i)  = root mean square wave height in meters
C     cg%WSETBC(i)  = wave setup in meters
C     cg%SWLBC(i)   = still water level in meters above the
C     datum used for the input bottom profile
C     cg%WANGBC(i)  = incident wave cg%ANGLE in degrees from shorenormal if cg%ILINE=1
C     and from reference direcction(e.g.,North) if cg%ILINE=2 or larger
C     For cg%IPROFL = 0, use input cg%TIMEBC(i+1)=1.0,2.0,... to identify
C     each combination of waves and still water level
C
C mg
C mg........INPUT WAVE and WATER LEVEL OPTION
C mg  cg%ILAB=0 for field data set - waves and water levels read separately
C mg  cg%ILAB=1 for laboratory data set - waves and water levels read together
C mg
C     READ(11,1110) cg%ILAB
      READ(11,*) DUM !bdj
      cg%ILAB=NINT(DUM) !bdj
C mg
C     READ(11,1110) cg%NWAVE
      READ(11,*) DUM  !bdj
      cg%NWAVE=NINT(DUM) !bdj
C     READ(11,1110) cg%NSURG
      READ(11,*) DUM  !bdj
      cg%NSURG=NINT(DUM) !bdj
C mg
      IF(cg%ILAB.EQ.1) THEN
C mg
        cg%NTIME=cg%NWAVE
        cg%TIMEBC(1) = 0.D0
        DO 120 I = 1,cg%NTIME
C         READ(11,1160) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),cg%WSETBC(I),
C    +       cg%SWLBC(I),cg%WANGBC(I)
C         lzhu change to cg%IVEG.EQ.3.AND.cg%IDISS.EQ.1/2/3
          IF (cg%IVEG.LE.2) THEN
             READ(11,*) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),
     +       cg%WSETBC(I), !bdj
     +       cg%SWLBC(I),cg%WANGBC(I)                             !bdj
          ENDIF

          IF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.1) THEN
             READ(11,*) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),
     +       cg%WSETBC(I), !bdj
     +                  cg%SWLBC(I),cg%WANGBC(I)                       !bdj
          ELSEIF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.2) THEN
             READ(11,*) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),
     +       cg%WSETBC(I), !lzhu
     +                  cg%SWLBC(I),cg%WANGBC(I),cg%FREQMINBC(I),
     +       cg%FREQMAXBC(I), !lzhu
     +                  cg%FREQNUMBC(I)
          ELSEIF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.3) THEN
             READ(11,*) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),
     +       cg%WSETBC(I), !lzhu
     +                  cg%SWLBC(I),cg%WANGBC(I),cg%FREQMINBC(I),
     +       cg%FREQMAXBC(I),
     +                  cg%NMEASSPEC
          ENDIF
C         IF(cg%WANGBC(I).LT.-80.D0.OR.cg%WANGBC(I).GT.80.D0) THEN
C           WRITE (*,2800) cg%WANGBC(I)
C 2800      FORMAT(/'Incident Wave cg%ANGLE=',D11.4,
C    +           'but cg%ANGLE must be in the range of -80
C    +to 80 in degree')
C           STOP
C         ENDIF
 120    CONTINUE
      ELSE
C
C     For field data,wave conditions(cg%NWAVE+1) and water level(cg%NSURG+1)
C     at x=0 vary continously in cg%TIME starting from cg%TIME=0 unlike
C     step changes assumed for laboratory data, defined by cg%ILAB=1.
C     Choose number of step changes to approximate cg%TIME series
        cg%NTIME=MAX0(cg%NWAVE,cg%NSURG)
        NWAVE1=cg%NWAVE+1
        DO 130 I=1,NWAVE1
C         READ(11,1170) TWAVE(I),TPIN(I),HRMSIN(I),WANGIN(I)
          IF (cg%IVEG.LE.2) THEN
             READ(11,*) TWAVE(I),TPIN(I),HRMSIN(I),WANGIN(I) !bdj
          ENDIF
          IF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.1) THEN
             READ(11,*) TWAVE(I),TPIN(I),HRMSIN(I),WANGIN(I) !bdj
          ELSEIF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.2) THEN
             READ(11,*) TWAVE(I),TPIN(I),HRMSIN(I),WANGIN(I), !lzhu
     +                   FREQMININ(I),FREQMAXIN(I),FREQNUMIN(I)
          ELSEIF (cg%IVEG.EQ.3.AND.cg%IDISS.EQ.3) THEN
             READ(11,*) TWAVE(I),TPIN(I),HRMSIN(I),WANGIN(I) !lzhu
          ENDIF
          IF(cg%NWAVE.EQ.cg%NTIME) cg%TIMEBC(I)=TWAVE(I)
 130    CONTINUE
 1170   FORMAT(D11.1,3D11.4)
        NSURG1=cg%NSURG+1
        DO 131 I=1,NSURG1
C         READ(11,1180) TSURG(I),SWLIN(I)
          READ(11,*) TSURG(I),SWLIN(I) !bdj
          IF(cg%NSURG.EQ.cg%NTIME) cg%TIMEBC(I)=TSURG(I)
 131    CONTINUE
 1180   FORMAT(D11.1,D11.4)
        IF(TWAVE(1).NE.0.D0.OR.TSURG(1).NE.0.D0) THEN
          WRITE(*,2801)
          STOP
 2801     FORMAT(/'Data input is stopped because the start
     +      cg%TIME for offshore wave conditions and water
     +      level is NOT ZERO'/)
        ENDIF
        IF(TWAVE(NWAVE1).NE.TSURG(NSURG1)) THEN
          WRITE(*,2802)
          STOP
 2802     FORMAT(/'Data input stopped because the durations
     +      of offshore wave conditions and water level
     +      are NOT SAME'/)
        ENDIF
C
C     Subr.19 TSINTP interpolates input cg%TIME series at
C     specified cg%TIME cg%TIMEBC(I) with I=1,2,...,(cg%NTIME+1) and
C     generates stepped cg%TIME series corresponding to input format
C     for the case of cg%NWAVE=cg%NSURG
        CALL TSINTP(cg%NWAVE,TWAVE,TPIN,cg%NTIME,cg%TIMEBC,cg%TPBC)
        CALL TSINTP(cg%NWAVE,TWAVE,HRMSIN,cg%NTIME,cg%TIMEBC,cg%HRMSBC)
        CALL TSINTP(cg%NWAVE,TWAVE,WANGIN,cg%NTIME,cg%TIMEBC,cg%WANGBC)
        CALL TSINTP(cg%NSURG,TSURG,SWLIN,cg%NTIME,cg%TIMEBC,cg%SWLBC)
C     lzhu add the following
        IF (cg%IVEG.EQ.3.AND.cg%IDISS.GE.2) THEN
           CALL TSINTP(cg%NWAVE,TWAVE,FREQMININ,cg%NTIME,cg%TIMEBC,
     +       cg%FREQMINBC)
           CALL TSINTP(cg%NWAVE,TWAVE,FREQMAXIN,cg%NTIME,cg%TIMEBC,
     +       cg%FREQMAXBC)
           CALL TSINTP(cg%NWAVE,TWAVE,FREQNUMIN,cg%NTIME,cg%TIMEBC,
     +       cg%FREQNUMBC)
        ENDIF
C     Wave setup at x=0 is assumed to be zero
        cg%WSETBC(1:cg%NTIME)=0.D0
C       DO 132 I=1,cg%NTIME
C         IF(cg%WANGBC(I).LT.-80.D0.OR.cg%WANGBC(I).GT.80.D0) THEN
C           WRITE(*,2800) cg%WANGBC(I)
C           STOP
C         ENDIF
C 132   CONTINUE
C
      ENDIF
C     End of field data input for (cg%ILAB=0)
C
C     Prepare for ITIME=1 computation
C     IF cg%IPOND=1, ponded water level cg%ZW=SWL at cg%TIME=0
      cg%TP = cg%TPBC(1)
      cg%HRMS(1) = cg%HRMSBC(1)
      cg%WSETUP(1) = cg%WSETBC(1)
      cg%ANGLE= cg%WANGBC(1)
C     lzhu add the following
      IF (cg%IVEG.EQ.3.AND.cg%IDISS.GE.2) THEN
         cg%FREQMIN=cg%FREQMINBC(1)
         cg%FREQMAX=cg%FREQMAXBC(1)
         cg%FREQNUM=cg%FREQNUMBC(1)
      ENDIF

      IF(cg%IPOND.EQ.1) cg%ZW=cg%SWLBC(1)
C
C     ......... BOTTOM GEOMETRY and POROUS LAYER BOTTOM if cg%IPERM=1
C     The bottom geometry is divided into segments of
C     different inclination and roughness starting from
C     seaward boundary for cross-shore line L.
C     cg%YLINE(L)  = alongshore coordinate for line L=1,2,...,cg%ILINE
C     cg%AGLINE(L) = cg%ANGLE of line L from reference direction(e.g.,North) but
C     cg%YLINE(1)=0.0 and cg%AGLINE(1)=0.0 if cg%ILINE=1
C     cg%NBINP(L)  = number of input points of bottom elevation cg%ZB(X)
C     cg%XBINP(J,L)= horizontal distance to input bottom point (J)
C     in meters where cg%XBINP(1,L) = 0 at the seaward boundary
C     cg%ZBINP(J,L)= dimensional vertical coordinate (+ above datum)
C     of input bottom point (J) in meters along cross-shore line L
C     cg%FBINP(J,L)= bottom friction factor for segment between
C     points (J) and (J+1) along cross-shore line L where if cg%IVEG=1,
C     cg%FBINP needs to be positive in vegetated zone
C     cg%WMINP(J,L)=1.0 for wire mesh segment between points (J) and (J+1)
C     along cross-shore line L for cg%ISEDAV=2 where cg%WMINP(J,L)=0.0 for no
C     wire mesh segment
C     cg%NPINP(L)  = number of input points of impermeable hard or clay bottom
C     cg%ZP(X) along cross-shore line L only if cg%IPERM=1 or cg%ISEDAV=1 but
C     for cg%ISTSAN=1,cg%ZP(X) is sand bottom elevation beneath stone structure
C     cg%XPINP(J,L)= horizontal distance of input point J from x=0
C     cg%ZPINP(J,L)= dimensional vertical coordinate in meters of
C     porous layer bottom or hard or clay bottom at point (J) with cg%ZPINP(J)
C     equal to or less than cg%ZBINP(J,L) where cg%ZPINP(1,L)=cg%ZBINP(1,L) imposed
C     If cg%ICLAY=1, clay resistance and sand fraction in clay are input
C     cg%RCINP(J,L)= clay resistance parameter of order of 10 m*m/s/s
C     cg%FCINP(J,L)= sand volume per unit clay volume in range of 0.0 to (1-SPORO)=cg%SPORO1
C     IF cg%ISEDAV = 1, an almost vertical impermeable wall can be specified
C     using two points (cg%NPINP-1) and cg%NPINP where
C     cg%IVWALL(L) = 0 for no vertical wall along cross-shore line L
C     cg%IVWALL(L) = 1 for vertical wall with sediment in front
C     cg%IVWALL(L) = 2 for vertical wall exposed to wave action
      DO 160 L=1, cg%ILINE
      cg%YLINE(L)=0.D0
      cg%AGLINE(L)=0.D0
C     IF(cg%ILINE.GT.1) READ(11,1150) cg%YLINE(L),cg%AGLINE(L)
      IF(cg%ILINE.GT.1) READ(11,*) cg%YLINE(L),cg%AGLINE(L) !bdj
C     READ(11,1110) cg%NBINP(L)
      READ(11,*) DUM       !bdj
      cg%NBINP(L) = NINT(DUM) !bdj
C     IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) READ(11,1110) cg%NPINP(L)
      IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
        READ(11,*) DUM       !bdj
        cg%NPINP(L) = NINT(DUM) !bdj
      ENDIF
      IF(cg%NBINP(L).GT.NN) THEN
        WRITE(*,2900) L, cg%NBINP(L), NN
        STOP
      ENDIF
 2900 FORMAT(/'Number of Input Bottom Nodes NBINP(',I3,') = ',I8,'
     +   ;NN = ',I8/'Increase PARAMETER NN.')
C
C     Point J = 1 has no corresponding friction factor.
C     READ (11,1150) cg%XBINP(1,L), cg%ZBINP(1,L)
      READ (11,*) cg%XBINP(1,L), cg%ZBINP(1,L) !bdj
      cg%XBINP(1,L) = 0.D0
      DO 140 J = 2,cg%NBINP(L)
C       READ(11,1150) cg%XBINP(J,L), cg%ZBINP(J,L), cg%FBINP(J-1,L)
        IF(cg%ISEDAV.LE.1) THEN
          READ(11,*) cg%XBINP(J,L), cg%ZBINP(J,L), cg%FBINP(J-1,L) !bdj
        ELSE
          READ(11,*) cg%XBINP(J,L), cg%ZBINP(J,L), cg%FBINP(J-1,L),
     +       cg%WMINP(J-1,L)
        ENDIF
C     IF cg%IANGLE = 1, the bottom friction factor must be positive
c     lzhu changed cg%IVEG.GE.1 to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.2
        IF(cg%ANGLE.NE.cg%AGLINE(L).OR.cg%IVEG.EQ.1.OR.cg%IVEG.EQ.2)
     +       THEN
          IF(cg%FBINP(J-1,L).LE.0.D0) THEN
            WRITE(*,2901) cg%FBINP(J-1,L), (J-1), L
            STOP
          ENDIF
        ENDIF
C     Avoid perfect horizontal bottom for possible numerical difficulty
        IF(cg%ZBINP(J,L).EQ.cg%ZBINP(J-1,L)) cg%ZBINP(J-1,L)=
     +       cg%ZBINP(J-1,L)-1.D-4
 140  CONTINUE
      DUM=cg%XBINP(cg%NBINP(L),L)/cg%DX
      IDUM=NINT(DUM)
      DUM=DUM-DBLE(IDUM)
      IF(DUM.LT.1.D-5) cg%XBINP(cg%NBINP(L),L)=cg%XBINP(cg%NBINP(L),L)
     +       +1.D-4
 2901 FORMAT(/'Bottom Friction Factor cg%FBINP(J-1,L)=', D11.4,
     +   'for (J-1) =',I4,'and L=',I3/'For obliquely incident
     +    wave or vegetated zone, cg%FBINP must be positive')
      IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
        cg%XPINP(1,L)=0.D0
        cg%ZPINP(1,L)=cg%ZBINP(1,L)
        DO 150 J=2,cg%NPINP(L)
          IF(cg%ICLAY.EQ.0)THEN
C             READ(11,1150) cg%XPINP(J,L),cg%ZPINP(J,L)
              READ(11,*) cg%XPINP(J,L),cg%ZPINP(J,L) !bdj
          ELSE
              READ(11,*) cg%XPINP(J,L),cg%ZPINP(J,L),cg%RCINP(J,L),
     +       cg%FCINP(J,L)
          ENDIF
 150    CONTINUE
        IF(cg%XPINP(cg%NPINP(L),L).LT.cg%XBINP(cg%NBINP(L),L))
     +       cg%XPINP(cg%NPINP(L),L)
     +    =cg%XBINP(cg%NBINP(L),L)
      ENDIF
      IF(L.GT.1) cg%DYLINE(L-1)=cg%YLINE(L)-cg%YLINE(L-1)
C
C.....VEGETATION CHARACTERISTICS IF cg%IVEG=1, 2, or 3
C     cg%VEGCD    = Vegetation drag coefficient of order of unity
C     cg%VEGN(J,L)= number of vegetation (1/m/m) per unit horizontal area
C     for segment J(J=1,2,...,cg%NBINP(L)-1) along cross-shore line L where
c     cg%VEGN(J,L)=0.0 if no vegetation
C     cg%VEGB(J,L)= width(m) of each vegetation stand where cg%VEGB(J,L)=0.0
C     if no vegetation
C     cg%VEGD(J,L)= height(m) of each vegetation stand above sand where
C     cg%VEGD(J,L)=0.0 if no vegetation
C     cg%VEGRD(J,L)=root depth (m) below sand for no vegetation uprooting
C     where uprooting occurs when erosion reaches this depth
C     (input only for cg%IVEG=1) where cg%VEGRD(J,L)=0.0 if no vegetation
      IF(cg%IVEG.GE.1) THEN
C     READ(11,1130) cg%VEGCD
      READ(11,*) cg%VEGCD

C     lzhu comments: only when cg%IVEG=3, Cdm is written in makeinfile and read here
      IF (cg%IVEG.EQ.3) READ(11,*) cg%VEGCDM
c     lzhu comments end

      JDUM=cg%NBINP(L)-1
      DO 170 J=1,JDUM
c       lzhu change to: cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
c       cg%IVEG=3 also needs to read vegetation properties specified in input file
        IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
          READ(11,*)cg%VEGN(J,L),cg%VEGB(J,L),cg%VEGD(J,L),
     +       cg%VEGRD(J,L)
        ELSE
C         READ(11,1150) cg%VEGN(J,L),cg%VEGB(J,L),cg%VEGD(J,L)
          READ(11,*) cg%VEGN(J,L),cg%VEGB(J,L),cg%VEGD(J,L)
        ENDIF
        cg%VEGINP(J,L)=cg%VEGCD*cg%VEGN(J,L)*cg%VEGB(J,L)/
     +       cg%FBINP(J,L)
        IF(cg%VEGINP(J,L).LT.0.D0) THEN
          WRITE(*,2902) cg%VEGINP(J,L),J,L
          STOP
        ENDIF
 170    CONTINUE
      ENDIF
 2902 FORMAT(/'Vegetation Input Characteristic VEGINP(J,L)
     +   =',F11.4,'for Segment J=',I4,'and Line L=',I3/'
     +   Vegetation CD,N and B must be positive or zero')
C
C.....DIKE GRASS AND SOIL CHARACTERISTICS IF cg%IPROFL=2
C     cg%GDINP(J,L)=thickness (m) of grass cover for segment J along cross-
C     shore line L where cg%GDINP(J,L)=0.0 if no grass cover
C     cg%GRINP(J,L)=grass surface resistance parameter (m*m/s/s)
C     cg%GRDINP(J,L)=resistance parameter (m*m/s/s) below grass cover
C     where grass resistance is assumed to decrease linearly downward
C     in grass cover and be (+) constant below grass cover
      IF(cg%IPROFL.EQ.2) THEN
        DO 180 J=1,cg%NBINP(L)-1
          READ(11,*) cg%GDINP(J,L),cg%GRINP(J,L),cg%GRDINP(J,L)
          IF(cg%GDINP(J,L).LT.0.D0) cg%GDINP(J,L)=0.D0
          IF(cg%GRDINP(J,L).LE.0.01D0) cg%GRDINP(J,L)=0.01D0
          IF(cg%GDINP(J,L).EQ.0.D0) cg%GRINP(J,L)=cg%GRDINP(J,L)
          IF(cg%GDINP(J,L).GT.0.D0) THEN
            IF(cg%GRINP(J,L).LE.cg%GRDINP(J,L)) cg%GRINP(J,L)=
     +       cg%GRDINP(J,L)+0.01D0
          ENDIF
 180    CONTINUE
      ENDIF
C
 160  CONTINUE
C     End of line L=1,2,...,cg%ILINE
C
C.....WIND SPEED AND DIRECTION IF cg%IWIND=1
C     During cg%TIME = cg%TIMEBC(i) to cg%TIME=cg%TIMEBC(i+1)
C     cg%W10(i) = wind speed (m/s) at 10m elevation above mean sea level
C     cg%WANGLE(i)= wind direction in degrees at 10 m
C     from a cross-shore line(not adjusted for a curved beach)
C     cg%WINDCD(i)= wind drag coefficient based on Large and Pond (1981)
C     cg%TWXSTA(i)= cross-shore wind shear stress/specific water weight
C     cg%TWYSTA(i)= longshore wind shear stress/specific water weight
C     RATIO = specific water weight/specific air weight
C
C     Wind data cg%TIME series is read in the same way as
C     field data of waves and water level
      IF(cg%IWIND.EQ.1) THEN
C       READ(11,1110) cg%NWIND
        READ(11,*) DUM    !bdj
        cg%NWIND = NINT(DUM) !bdj
        NWIND1=cg%NWIND+1
        DO 190 I=1,NWIND1
C         READ(11,1190) TWIND(I),WIND10(I),WINDAN(I)
          READ(11,*) TWIND(I),WIND10(I),WINDAN(I) !bdj
 190    CONTINUE
 1190   FORMAT(D11.1,2D11.4)
        IF(TWIND(1).NE.0.D0) THEN
          WRITE(*,2905)
          STOP
 2905     FORMAT(/'Data input is stopped because the start TIME of
     +       wind data is NOT ZERO'/)
        ENDIF
        IF(TWIND(NWIND1).NE.cg%TIMEBC(cg%NTIME+1)) THEN
          WRITE(*,2906)
          STOP
 2906     FORMAT(/'Data input is stopped because the end TIME of
     +       wind data is NOT SAME as the end TIME of
     +       wave and water level data'/)
        ENDIF
        CALL TSINTP(cg%NWIND,TWIND,WIND10,cg%NTIME,cg%TIMEBC,cg%W10)
        CALL TSINTP(cg%NWIND,TWIND,WINDAN,cg%NTIME,cg%TIMEBC,cg%WANGLE)
        RATIO = 837.D0
        CONVRT = 3.1415926D0/180.D0
        DO 200 I=1,cg%NTIME
          IF(cg%W10(I).GT.25.D0) WRITE(*,2910)
          IF(cg%W10(I).LT.11.D0) THEN
            cg%WINDCD(I) = 1.2D-3
          ELSE
            cg%WINDCD(I)=0.49D-3 + 0.065D-3*cg%W10(I)
          ENDIF
          DUM = (cg%WINDCD(I)/RATIO/cg%GRAV)*cg%W10(I)*cg%W10(I)
          ANG = CONVRT*cg%WANGLE(I)
          cg%TWXSTA(I)=DUM*DCOS(ANG)
          cg%TWYSTA(I)=DUM*DSIN(ANG)
 200    CONTINUE
      ELSE
        DO 201 I=1,cg%NTIME
          cg%TWXSTA(I) = 0.D0
          cg%TWYSTA(I) = 0.D0
 201    CONTINUE
      ENDIF
 2910 FORMAT(/'Wind speed at 10m =',D11.4/
     +   'but wind speed must be less than 25m/s for
     +   Large and Pond(1981)')
C
C.....LANDWARD STILL WATER LEVEL IF cg%IWTRAN=1
C     During cg%TIME=cg%TIMEBC(i) to cg%TIME=cg%TIMEBC(i+1)
C     cg%SWLAND(i)=still water level in meters above datum
C     landward of emerged structure or dune if cg%IOFLOW=0
C     If cg%ISWLSL=0, seaward and landward still water levels
C     are same and cg%SWLAND(i)=cg%SWLBC(i)
C     If cg%ISWLSL=1, read cg%TIME series of landward still water level
C     SLANIN(I) at cg%TIME TSLAND(I) with I=1,2,...,(NSLAN+1)
C     If cg%ISWLSL=2, no water landward of structure or dune and overflow
C     (cg%IOFLOW=1) occurs if crest is submerged
      IF(cg%IWTRAN.EQ.1) THEN
C       READ(11,1110) cg%ISWLSL
        READ(11,*) DUM !bdj
        cg%ISWLSL = NINT(DUM) !bdj
        IF(cg%ISWLSL.EQ.0) THEN
          DO 300 I=1,cg%NTIME
            cg%SWLAND(I)=cg%SWLBC(I)
 300      CONTINUE
        ENDIF
        IF(cg%ISWLSL.EQ.1) THEN
C         READ(11,1110) NSLAN
          READ(11,*) DUM !bdj
          NSLAN = NINT(DUM) !bdj
          NSLAN1=NSLAN+1
          DO 301 I=1,NSLAN1
C           READ(11,1180) TSLAND(I),SLANIN(I)
            READ(11,*) TSLAND(I),SLANIN(I) !bdj
 301      CONTINUE
          IF(TSLAND(1).NE.0.D0) THEN
            WRITE(*,2950)
            STOP
 2950       FORMAT(/'Data input is stopped because the start TIME of
     +landward SWL is NOT ZERO'/)
          ENDIF
          IF(TSLAND(NSLAN1).NE.cg%TIMEBC(cg%NTIME+1)) THEN
            WRITE(*,2951)
            STOP
 2951       FORMAT(/'Data input is stopped because the end TIME of
     +        landward SWL is NOT SAME as the end TIME of
     +        other input TIME series'/)
          ENDIF
          CALL TSINTP(NSLAN,TSLAND,SLANIN,cg%NTIME,cg%TIMEBC,cg%SWLAND)
        ENDIF
      ENDIF
C
C.....ALONGSHORE WATER LEVEL GRADIENT IF cg%ITIDE=1
C     During cg%TIME=cg%TIMEBC(i) to cg%TIME=cg%TIMEBC(i+1)
C     cg%DETADY(i) = alongshore water level gradient for longshore current
C     cg%DSWLDT(i) = rate of input water level change only for cg%ILAB=0
C
C     Alongshore gradient data cg%TIME series is read in the same way as
C     field surge data
      IF(cg%ITIDE.EQ.1) THEN
C       READ(11,1110) NTIDE
        READ(11,*) DUM    !bdj
        NTIDE = NINT(DUM) !bdj
        NTIDE1=NTIDE+1
        DO 400 I=1,NTIDE1
C         READ(11,1195) TTIDE(I),DEDYIN(I)
          READ(11,*) TTIDE(I),DEDYIN(I) !bdj
 400    CONTINUE
 1195   FORMAT(D11.1,D11.7)
        IF(TTIDE(1).NE.0.D0) THEN
          WRITE(*,2961)
          STOP
 2961     FORMAT(/'Data input is stopped because the start TIME
     +       of tide data is NOT ZERO'/)
        ENDIF
        IF(TTIDE(NTIDE1).NE.cg%TIMEBC(cg%NTIME+1)) THEN
          WRITE(*,2962)
          STOP
 2962     FORMAT(/'Data input is stopped because the end
     +      TIME of tide data is NOT SAME as the end TIME of
     +      wave and water level data'/)
        ENDIF
        CALL TSINTP(NTIDE,TTIDE,DEDYIN,cg%NTIME,cg%TIMEBC,cg%DETADY)
      ENDIF
C
C     If cg%ITIDE=1 and cg%ILAB=0, cross-shore water flux associated
C     with continuous input water level change is accounted
C     for in cross-shore current in wet zone
      IF(cg%ITIDE.EQ.1.AND.cg%ILAB.EQ.0) THEN
        DO 410 I=1,cg%NSURG
          K=I+1
          DSDTIN(I)=(SWLIN(K)-SWLIN(I))/(TSURG(K)-TSURG(I))
  410   CONTINUE
        DSDTIN(NSURG1)=DSDTIN(cg%NSURG)
        CALL TSINTP(cg%NSURG,TSURG,DSDTIN,cg%NTIME,cg%TIMEBC,cg%DSWLDT)
      ENDIF
C
      CLOSE (11)
C
 1110 FORMAT (I8)
 1120 FORMAT (14A5)
 1130 FORMAT (D11.4)
 1150 FORMAT (4D11.4)
 1160 FORMAT (D11.1,5D11.4)
C
      RETURN
      END SUBROUTINE INPUT
C
C     -02-----------------  END OF SUBROUTINE INPUT  --------------------


c------------------------------------------------------------------------
      SUBROUTINE TSINTP(N1,T1,W1,N2,T2,W2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      integer, PARAMETER :: NB=30000
      DIMENSION T1(NB),W1(NB),T2(NB),W2(NB),F(NB)
C
      F(1)=W1(1)
      F(N2+1)=W1(N1+1)
      IF(N2.GE.2) THEN
        DO 100 K=2,N2
          DO 200 I=1,N1
            I1=I+1
            IF(T1(I).LE.T2(K).AND.T2(K).LT.T1(I1)) THEN
              DUM=(T2(K)-T1(I))/(T1(I1)-T1(I))
              F(K)=(1.D0-DUM)*W1(I)+DUM*W1(I1)
              GOTO 100
            ENDIF
 200      CONTINUE
 100    CONTINUE
      ENDIF
C
      DO 300 K=1,N2
        W2(K)=0.5D0*(F(K)+F(K+1))
 300  CONTINUE
C
      RETURN
      END SUBROUTINE TSINTP
c------------------------------------------------------------------------
c------------------------------------------------------------------------

C     #03####################  SUBROUTINE BOTTOM  ########################
C
C     This subroutine calculates the bottom geometry using input
C     DX between two adjacent nodes along ILINE cross-shore lines
C     Smooth input ZB(J,L) to reduce numerical irregularity
C
      SUBROUTINE BOTTOM (cg)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000, NB=30000, NL=100)
      DIMENSION SLOPE(NN), PSLOPE(NN), ZBRAW(NN), ZPRAW(NN)
C
C     The structure geometry is divided into segments of different
C     inclination and roughness for each cross-shore line L.
C     cg%NBINP(L) = number of input bottom points
C     For segments starting from the seaward boundary:
C     SLOPE(K)  = slope of segment K(+ upslope, - downslope)
C     cg%FBINP(K,L)  = bottom friction factor
C     cg%XBINP(K,L)  = dimensional horizontal distance from seaward
C     boundary to the seaward end of segment K
C     cg%ZBINP(K,L)  = dimensional vertical coordinate (+ above datum)
C     at the seaward end of segment K
C     PSLOPE(K) = slope of porous layer bottom or hard bottom
C     cg%XPINP(K,L)  = dimensional horizontal distance of porous layer bottom
C     at the seaward end of segment K
C     cg%ZPINP(K,L)  = dimensional vertical coordinate of porous layer bottom
C     at the seaward end of segment K
C
      DO 100 L = 1,cg%ILINE
      DO 120 K = 1,cg%NBINP(L)-1
        DUM=cg%XBINP(K+1,L)-cg%XBINP(K,L)
        SLOPE(K) = (cg%ZBINP(K+1,L)-cg%ZBINP(K,L))/DUM
 120  CONTINUE
C     No  vertical wall at landward end unless cg%IVWALL=1 or 2
      cg%IVWALL(L)=0
      IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
        DO 121 K=1,cg%NPINP(L)-1
          DUM=cg%XPINP(K+1,L)-cg%XPINP(K,L)
          PSLOPE(K)=(cg%ZPINP(K+1,L)-cg%ZPINP(K,L))/DUM
 121    CONTINUE
        IF(PSLOPE(cg%NPINP(L)-1).GT.cg%TANPHI) cg%IVWALL(L)=1
      ENDIF
C
C     ... INITIAL SHORELINE LOCATION AT DATUM Z=0
C
C     cg%XS(L)= horizontal distance between X=0 and shoreline
      K = 0
 900  CONTINUE
      K=K+1
      IF(K.EQ.cg%NBINP(L)) THEN
        cg%XS(L) = cg%XBINP(cg%NBINP(L),L)
        GOTO 901
      ENDIF
      CROSS = cg%ZBINP(K,L)*cg%ZBINP(K+1,L)
      IF (CROSS.GT.0.D0) GOTO 900
      cg%XS(L)  = cg%XBINP(K+1,L) - cg%ZBINP(K+1,L)/SLOPE(K)
 901  IF(L.EQ.1) THEN
      cg%DXD2 = cg%DX/2.D0
      cg%DX2 = 2.D0*cg%DX
      cg%DXDX = cg%DX*cg%DX
C
C     cg%NPT= integer used in Subr.14 SMOOTH
C     cg%NPE= integer used in Subr.15 EXTRAPO
C BDJ 2011->2014 on 2014-10-02
          cg%NPT=1+NINT(maxval(cg%HRMSBC)/cg%DX)
          cg%NPE=1+NINT(maxval(cg%HRMSBC)/cg%DX2)
C      cg%NPT=1+NINT(cg%HRMS(1)/cg%DX)
C      cg%NPE=1+NINT(cg%HRMS(1)/cg%DX2)
C END BDJ 2011->2014 on 2014-10-02
C     IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.1) cg%NPT=cg%NPT+2*NINT(cg%SDP/cg%DX)
      ENDIF
C
C     ... CALCULATE BOTTOM GEOMETRY AT EACH NODE
C
C     cg%JMAX(L) = landward edge node corresponding to maximum node number
C     cg%XB(J)= horizontal coordinate of node J where cg%XB(1) = 0
C     cg%ZB(J,L)= vertical coordinate of bottom at node J (+ above datum)
C     cg%BSLOPE(J,L) = bottom slope at node J for cross-shore line L
C     SLOPE(K) = tangent of local slope of segment K
C
      DUM = cg%XBINP(cg%NBINP(L),L)/cg%DX
      cg%JMAX(L)  = NINT(DUM)+1
      DUM=cg%DX*DBLE(cg%JMAX(L)-1)-cg%XBINP(cg%NBINP(L),L)
      IF(DUM.GT.0.D0) cg%JMAX(L)=cg%JMAX(L)-1
      IF(cg%JMAX(L).GT.NN) THEN
        WRITE (*,2910) L,cg%JMAX(L),NN
        STOP
      ENDIF
 2910 FORMAT (/' End Node of Line',I3,':JMAX(L)=',I8,'; NN =',I8/
     +   ' Bottom length is too long.'/
     +   ' Cut it, or change PARAMETER NN.')
C
C     INTERPOLATION OF BOTTOM POSITION at cg%XB(J)
C     cg%RCREST(L) = crest (highest) elevation above datum Z=0
C     cg%JCREST(L) = nodal location of crest for cross-shore Line L
C     If cg%IPOND=1, cg%JCREST(L)=nodal location of ridge crest computed in Subr.21 PONDED
      IF(L.EQ.1) JDUM=cg%JMAX(L)
      IF(cg%JMAX(L).LT.JDUM) GOTO 130
      JDUM=cg%JMAX(L)
      DO 141 J = 1,cg%JMAX(L)
        cg%XB(J) = cg%DX*DBLE(J-1)
 141  CONTINUE
 130  CONTINUE
      ZBRAW(1) = cg%ZBINP(1,L)
      cg%FB2(1,L)=0.5D0*cg%FBINP(1,L)
      IF(cg%ISEDAV.EQ.2) cg%WMNODE(1,L)=cg%WMINP(1,L)
      IF(cg%IVEG.GE.1) THEN
        cg%VEGFB(1,L)=cg%VEGINP(1,L)
        cg%VEGH(1,L)=cg%VEGD(1,L)
c       lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
c       cg%IVEG=3 and cg%IVEG=1 share similarity in specifying vegetatin properties,
c       except that cg%IVEG=3 allows choosing dissipation model and drag force model
        IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) cg%VEGRH(1,L)=cg%VEGRD(1,L)
      ENDIF
      IF(cg%IPROFL.EQ.2) THEN
        cg%GRSD(1,L)=cg%GDINP(1,L)
        cg%GRSR(1,L)=cg%GRINP(1,L)
        cg%GRSRD(1,L)=cg%GRDINP(1,L)
      ENDIF
      IF(cg%ICLAY.EQ.1) THEN
          cg%RCLAY(1,L)=cg%GRAV/cg%RCINP(1,L)
          cg%FCLAY(1,L)=1.D0-cg%FCINP(1,L)/cg%SPORO1
      ENDIF
      cg%RCREST(L) = ZBRAW(1)
      DO 142 J = 2, cg%JMAX(L)
        DO 143 K = 1, cg%NBINP(L)-1
          IF((cg%XB(J).GT.cg%XBINP(K,L)).AND.
     +      (cg%XB(J).LE.cg%XBINP(K+1,L))) THEN
            ZBRAW(J) = cg%ZBINP(K,L) + (cg%XB(J)-cg%XBINP(K,L))*SLOPE(K)
            cg%FB2(J,L) = 0.5D0*cg%FBINP(K,L)
            IF(cg%ISEDAV.EQ.2) cg%WMNODE(J,L)=cg%WMINP(K,L)
            IF(cg%IVEG.GE.1) THEN
              cg%VEGFB(J,L)=cg%VEGINP(K,L)
              cg%VEGH(J,L)=cg%VEGD(K,L)
c             lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
              IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) cg%VEGRH(J,L)=
     +          cg%VEGRD(K,L)
            ENDIF
            IF(cg%IPROFL.EQ.2) THEN
              cg%GRSD(J,L)=cg%GDINP(K,L)
              cg%GRSR(J,L)=cg%GRINP(K,L)
              cg%GRSRD(J,L)=cg%GRDINP(K,L)
            ENDIF
            GOTO 144
          ENDIF
 143    CONTINUE
 144    DUM = ZBRAW(J) - cg%RCREST(L)
        IF(cg%IPROFL.EQ.0.AND.DUM.GE.0.D0) THEN
          cg%RCREST(L) = ZBRAW(J)
          cg%JCREST(L) = J
        ENDIF
        IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
          IF(J.EQ.2) ZPRAW(1)=cg%ZPINP(1,L)
          DO 145 K=1,cg%NPINP(L)-1
            IF((cg%XB(J).GT.cg%XPINP(K,L)).AND.
     +         (cg%XB(J).LE.cg%XPINP(K+1,L))) THEN
              ZPRAW(J)=cg%ZPINP(K,L)+(cg%XB(J)-cg%XPINP(K,L))*PSLOPE(K)
              IF(cg%ICLAY.EQ.1) THEN
                  cg%RCLAY(J,L)=cg%GRAV/cg%RCINP(K,L)
                  cg%FCLAY(J,L)=1.D0-cg%FCINP(K,L)/cg%SPORO1
              ENDIF
              GOTO 142
            ENDIF
 145      CONTINUE
        ENDIF
 142  CONTINUE
      IF(cg%IVEG.GE.1)THEN
        DO 146 J=1,cg%JMAX(L)
          cg%VEGINP(J,L)=cg%FB2(J,L)*cg%VEGFB(J,L)
          IF(cg%IVEG.EQ.1) THEN
            IF(cg%VEGFB(J,L).EQ.0.D0) THEN
              cg%UPROOT(J,L)=0.D0
            ELSE
              cg%UPROOT(J,L)=1.D0
            ENDIF
          ENDIF
 146    CONTINUE
      ENDIF
C     cg%VEGFB(J,L) used in wet zone (Main Program) and cg%VEGINP(J,L) used
C     in wet and dry zone (Subr.16 WETDRY). cg%UPROOT(J,L)=0.0 in zone
C     of no vegetation or uprooted vegetation
C
C     Smooth ZBRAW(J) and ZPRAW(J) J=1-cg%JMAX(L) using Subr.14 SMOOTH
      JMAXL=cg%JMAX(L)
      CALL SMOOTH(cg, JMAXL,ZBRAW,SLOPE)
      IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) CALL
     +                              SMOOTH(cg, JMAXL,ZPRAW,PSLOPE)
      DO 149 J=1,cg%JMAX(L)
        cg%ZB(J,L)=SLOPE(J)
c bdj 2018-08-28  added to defeat the bottom smoothing for EMRRP model
        if(cg%ISMOOTH.eq.0)        cg%ZB(J,L)=ZBRAW(J)
c end bdj 2018-08-28  added to defeat the bottom smoothing for EMRRP model
        IF(cg%IPROFL.GE.1) cg%ZB0(J,L)=cg%ZB(J,L)
        IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) cg%ZP(J,L)=PSLOPE(J)
        IF(cg%ICLAY.EQ.1) cg%ZP0(J,L)=cg%ZP(J,L)
        IF(cg%ISEDAV.EQ.2) THEN
          IF(cg%WMNODE(J,L).LE.0.D0) THEN
            cg%ZMESH(J,L)=cg%ZP(J,L)
          ELSE
            cg%ZMESH(J,L)=cg%ZB(J,L)
          ENDIF
        ENDIF
 149  CONTINUE
C     Calculate bottom slope and cg%JCREST(if cg%IPROFL=1 or 2) using
C     smoothed cg%ZB(J)
      cg%BSLOPE(1,L) = (cg%ZB(2,L) - cg%ZB(1,L))/cg%DX
      JMAXM1 = cg%JMAX(L) - 1
      cg%BSLOPE(cg%JMAX(L),L) = (cg%ZB(cg%JMAX(L),L) -
     +                           cg%ZB(JMAXM1,L))/cg%DX
      DO 150 J=2,JMAXM1
        cg%BSLOPE(J,L) = (cg%ZB(J+1,L) - cg%ZB(J-1,L))/cg%DX2
 150  CONTINUE
      IF(cg%IPROFL.GE.1.AND.cg%IPOND.EQ.0) THEN
        cg%RCREST(L)=cg%ZB(1,L)
        DO 151 J=2,cg%JMAX(L)
          DUM=cg%ZB(J,L)-cg%RCREST(L)
          IF(DUM.GE.0.D0) THEN
            cg%RCREST(L)=cg%ZB(J,L)
            cg%JCREST(L)=J
          ENDIF
 151    CONTINUE
      ENDIF
C
C     cg%HP(J,L) = vertical thickness of porous or sediment layer
      IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
        DO 210 J=1,cg%JMAX(L)
          cg%HP(J,L) = cg%ZB(J,L) - cg%ZP(J,L)
          IF(cg%HP(J,L).LT.0.D0) THEN
            cg%HP(J,L)=0.D0
            cg%ZB(J,L)=cg%ZP(J,L)
          ENDIF
          IF(cg%ISTSAN.EQ.1) THEN
              cg%ZBSTON(J,L)=cg%ZB(J,L)
              cg%ZPSTON(J,L)=cg%ZP(J,L)
              cg%HPSTON(J,L)=cg%HP(J,L)
          ENDIF
 210    CONTINUE
      ENDIF
C
C     If cg%IVEG=1 or 3, cg%VEGZD(J,L) and cg%VEGZR(J,L) are the upper and lower
C     elevations of non-uprooted vegetation at node J and line L
c     lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
      IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
        DO 220 J=1,cg%JMAX(L)
          cg%VEGZD(J,L)=cg%ZB0(J,L)+cg%VEGH(J,L)
          cg%VEGZR(J,L)=cg%ZB0(J,L)-cg%VEGRH(J,L)
 220    CONTINUE
      ENDIF
C     where cg%VEGZD(J,L) and cg%VEGZR(J,L) are the same as the initial bottom
C     elevation cg%ZB0(J,L) in the zone of no vegetation with cg%VEGH(J,L)=0.0
C     and cg%VEGRH(J,L)=0.0
C
 100  CONTINUE
C
      RETURN
      END SUBROUTINE BOTTOM
C
C     -03----------------  END OF SUBROUTINE BOTTOM  ---------------------
c------------------------------------------------------------------------

c------------------------------------------------------------------------
C     #14##################### SUBROUTINE SMOOTH ##########################
C     Smooth the vector RAW using NPT computed in Subr.3 BOTTOM
C     where NPFS = (2NPT+1) = number of points for smoothing
      SUBROUTINE SMOOTH(cg,NUM,RAW,F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000)
      DIMENSION RAW(NN),F(NN)
C
      DO 201 J = 1,NUM
        JSTA = J-cg%NPT
        JEND = J+cg%NPT
        IF(JSTA.LT.1) THEN
          JSTA = 1
          JEND = 2*J-1
        ENDIF
        IF(JEND.GT.NUM) THEN
          JSTA = 2*J - NUM
          JEND = NUM
        ENDIF
        NPFS = JEND-JSTA+1
        TOTJ = DBLE(NPFS)
        SUM1 = 0.D0
        DO 202 JJ = JSTA, JEND
          SUM1 = SUM1+ RAW(JJ)
 202    CONTINUE
        F(J) = SUM1/TOTJ
 201  CONTINUE
C
      RETURN
      END SUBROUTINE SMOOTH
C     -14-----------------  END OF SUBROUTINE SMOOTH  ---------------------
c------------------------------------------------------------------------

C     #04#####################  SUBROUTINE PARAM  ########################
C
C     This subroutine calculates parameters used in other subroutines
C
      SUBROUTINE PARAM (cg)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NL=100)
C
C
C     ... CONSTANTS and PARAMETER
C
C     cg%PI      = 3.14159
C     cg%TWOPI   = 2.D0 * cg%PI
C     cg%GRAV    = acceleration due to gravity specified in Subr.2 INPUT
C     cg%SQR2    = Sqrt(2)
C     cg%SQR8    = Sqrt(8)
C     cg%SQRG1   = Sqrt(2/cg%PI)
C     cg%SQRG2   = 2*Sqrt(2/cg%PI)
C     cg%WKPO    = deep water wave number for the representative period
C
      cg%PI = 3.1415926D0
      cg%TWOPI = 2.D0*cg%PI
      cg%SQR2 = DSQRT(2.D0)
      cg%SQR8 = DSQRT(8.D0)
      cg%SQRG1= DSQRT(2.D0/cg%PI)
      cg%SQRG2= 2.D0*cg%SQRG1
      cg%WKPO = (cg%TWOPI)**2.D0/(cg%GRAV*cg%TP**2.D0)
C
C.....POROUS FLOW RESISTANCE PARAMETERS IF cg%IPERM=1
C     cg%SNP = stone porosity specified in Subr.2 INPUT
C     cg%SDP = nominal stone diameter specified in Subr.2 INPUT
C     cg%WNU = kinematic viscosity of water (m*m/s)
C     cg%WPM = maximum seepage velocity (m/s)
C     If cg%INFILT=1, cg%WPM is computed using cg%SNP=SPORO and cg%SDP=cg%D50 of sand
C     in Subr.2 INPUT
      IF(cg%IPERM.EQ.1.OR.cg%INFILT.EQ.1) THEN
        cg%WNU = 1.0D-6
        A = 1000.D0
        B = 5.D0
        IF(cg%IPERM.EQ.1) THEN
          DUMP=cg%SNP
          DUMD=cg%SDP
        ENDIF
        IF(cg%INFILT.EQ.1) THEN
          DUMP=1.D0-cg%SPORO1
          DUMD=cg%D50
        ENDIF
        C = 1.D0 - DUMP
        cg%ALPHA = A*cg%WNU*C**2.D0/(DUMP*DUMD)**2.D0
        cg%BETA1 = B*C/DUMP**3.D0/DUMD
        cg%BETA2 = 7.5D0*B*C/cg%SQR2/DUMP**2.D0
C     Need to divide cg%BETA2 by cg%WT(J) in Subr.9 POFLOW
        cg%ALSTA  = cg%ALPHA/cg%GRAV
        cg%BESTA1 = cg%BETA1/cg%GRAV
        cg%BESTA2 = cg%BETA2/cg%GRAV
        cg%ALSTA2 = cg%ALSTA*cg%ALSTA
        cg%BE2    = 2.D0*cg%BESTA1
        cg%BE4    = 2.D0*cg%BE2
        cg%WPM    = (DSQRT(cg%ALSTA2+cg%BE4)-cg%ALSTA)/cg%BE2
      ENDIF
C
C.....SWASH PARAMETERS IN WET AND DRY ZONE IF cg%IOVER=1
C     cg%AWD = swash velocity parameter
C     cg%AWD=2.0 calibrated for structures (cg%IPROFL=0 or cg%IPERM=1)
C     cg%AWD=1.6 calibrated for wave overwash of sand dunes
C     cg%EWD = duration-based exceedance probability for output
C     where cg%AWD has not been calibrated extensively and
C     cg%EWD=0.01-0.02 approximately corresponds to 2-percent exceedance
C     probability based on individual overtopping events.
      IF(cg%IOVER.EQ.1) THEN
        cg%AWD=2.0D0
        IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.0) cg%AWD=1.6D0
        cg%EWD = 0.015D0
        IF(cg%IPERM.EQ.1) cg%EWD=0.01D0
C     The following parameters are constant in Subr.16 WETDRY
        cg%CWD= 0.75D0*DSQRT(cg%PI)
        cg%AQWD = cg%CWD*cg%AWD
        cg%AGWD = cg%AWD*cg%AWD
        cg%AUWD = 0.5D0*DSQRT(cg%PI)*cg%AWD
        cg%BWD = (2.D0-9.D0*cg%PI/16.D0)*cg%AGWD + 1.D0
      ENDIF
C
      RETURN
      END SUBROUTINE PARAM
C
C     -04-----------------  END OF SUBROUTINE PARAM  ---------------------


C     #08#####################  SUBROUTINE OUTPUT  ########################
C
C     This subroutine stores computed and input quantities
C
      SUBROUTINE OUTPUT(cg,ITIME,L,ITEQO,ICONV)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000, NB=30000,NL=100)
      DIMENSION DUMVEC(NN),EDEPTH(NN)
C
C
C     ......... OUTPUT ONLY WHEN ITIME=0.............................
C
      IF(ITIME.EQ.0) THEN
      IF(L.GT.1) GOTO 888
C
C     ......... COMPUTATIONAL OPTION
C     cg%ILINE=number of cross-shore lines
      WRITE(20,890) cg%ILINE,cg%IQYDY
 890  FORMAT('COMPUTATION OPTION ILINE=',I3/
     +    'Alongshore gradient IQYDY=',I3/
     +    'ILINE cross-shore lines are computed together'/)
C
C     cg%IPROFL=0 for fixed bottom profile
C     cg%IPROFL=1 for profile evolution computation
C     cg%IPROFL=2 for dike erosion computation
        IF(cg%IPROFL.EQ.0) THEN
          WRITE(20,900) cg%IPROFL
        ENDIF
 900    FORMAT('COMPUTATION OPTION IPROFL =',I3/
     +     'Bottom profile is fixed and No sediment transport
     +     is computed'/)
C
        IF(cg%IPROFL.EQ.1) THEN
          WRITE(20,901) cg%IPROFL,cg%TIMEBC(cg%NTIME+1),cg%NTIME
          IF(cg%ISEDAV.EQ.1.AND.cg%ICLAY.EQ.0) WRITE(20,902)
     +          cg%ISEDAV,cg%BEDLM
          IF(cg%ICLAY.EQ.1) WRITE(20,904) cg%ICLAY,cg%BEDLM,cg%DEEB,
     +          cg%DEEF
          IF(cg%ISEDAV.EQ.2) WRITE(20,905) cg%ISEDAV,cg%BEDLM
        ENDIF
 901    FORMAT('COMPUTATION OPTION IPROFL =',I3/
     +     'Profile evolution is computed from TIME = 0.0'/
     +     'to TIME = ',F13.1,'  for NTIME = ', I4/)
 902    FORMAT('ISEDAV=',I3,' for hard bottom with',
     +    'bedload reduction factor BEDLM=',F4.1/)
 904    FORMAT('ICLAY=',I3,'for sand on erodible clay bottom'/
     +    'with bedload reduction factor BEDLM=',F4.1/
     +    'erosion efficiency DEEB=',F6.4/
     +    'erosion efficiency DEEF=',F6.4/)
 905    FORMAT('ISEDAV=',I3,' for wire mesh with',
     +    'bedload reduction factor BEDLM=',F4.1/)
C
        IF(cg%IPROFL.EQ.2) THEN
          WRITE(20,903) cg%IPROFL,cg%TIMEBC(cg%NTIME+1),cg%NTIME,
     +          cg%DEEB,cg%DEEF
        ENDIF
 903    FORMAT('COMPUTATIONAL OPTION IPROFL=',I3/
     +     'Dike erosion is computed from TIME=0.0'/
     +     'to TIME=',F13.1,'  for NTIME=',I4/
     +     'Efficiency DEEB=',F6.4/
     +     'Efficiency DEEF=',F6.4/)
C
        IF(cg%IROLL.EQ.0) WRITE(20,910)
        IF(cg%IROLL.EQ.1) WRITE(20,911) cg%RBZERO
 910    FORMAT('NO ROLLER is included in computation'/)
 911    FORMAT('ROLLER is included in computation'/
     +     'ROLLER slope Betazero =', F9.3/)
C
        IF(cg%IWCINT.EQ.0) WRITE(20,920)
        IF(cg%IWCINT.EQ.1) WRITE(20,921)
 920    FORMAT('NO wave and current interaction included'/)
 921    FORMAT('WAVE and current interaction included'/)
C
        IF(cg%IOVER.EQ.0) WRITE(20,930)
        IF(cg%IOVER.EQ.1.AND.cg%IPOND.EQ.0) THEN
          WRITE(20,931) cg%RWH,cg%JCREST(L),cg%RCREST(L),cg%AWD,cg%EWD
        ENDIF
        IF(cg%IOVER.EQ.1.AND.cg%IPOND.EQ.1) WRITE(20,932) cg%RWH,
     +          cg%AWD,cg%EWD,cg%ZW
 930    FORMAT('NO wave overtopping, overflow and seepage'/)
 931    FORMAT('WAVE OVERTOPPING, OVERFLOW AND SEEPAGE'/
     +     'Runup wire height (m)               RWH=',F9.3/
     +     'Initial crest location for L=1      JCREST=',I6/
     +     'Initial crest height (m) for L=1    RCREST=',F9.3/
     +     'Swash velocity parameter            AWD=',F9.3/
     +     'Output exceedance probability       EWD=',F9.3/)
 932    FORMAT('PONDED WATER IN RUNNEL'/
     +   'Runup wire height (m)               RWH=',F9.3/
     +   'Swash velocity parameter            AWD=',F9.3/
     +   'Output exceedance probability       EWD=',F9.3/
     +   'Initial ponded water level (m)       ZW=',F9.3/)
C
        IF(cg%IPERM.EQ.0) WRITE(20,940)
        IF(cg%IPERM.EQ.1) WRITE(20,941) cg%SNP,cg%SDP,cg%CSTABN,cg%WNU,
     +          cg%WPM
        IF(cg%ISTSAN.EQ.1) WRITE(20,942) cg%CPSTON
 940    FORMAT('IMPERMEABLE BOTTOM assumed'/)
 941    FORMAT('PERMEABLE BOTTOM consisting of'/
     +     'Stone porosity                       SNP=',F9.3/
     +     'Nominal stone diameter (m)         DN50=',F9.4/
     +     'Critical stability number        CSTABN=',F9.3/
     +     'Water kinematic viscosity(m*m/s)       =',F9.7/
     +     'Maximum seepage velocity (m/s)      WPM=',F9.5/)
 942    FORMAT('ISTSAN=1 for fixed stone structure on sand bottom'/
     +     'Empirical parameter CPSTON=',F5.2/)
C
        IF(cg%IWIND.EQ.0) WRITE(20,950)
        IF(cg%IWIND.EQ.1) WRITE(20,951) cg%NWIND
 950    FORMAT('NO wind shear stresses included'/)
 951    FORMAT('WIND shear stresses in momentum equations'/
     +     'Number of wind speed and direction input =',I4/)
C
        IF(cg%IVEG.EQ.0) WRITE(20,955)
        IF(cg%IVEG.EQ.1) WRITE(20,956) cg%VEGCD
        IF(cg%IVEG.EQ.2) WRITE(20,957) cg%VEGCD
c       lzhu added
        IF(cg%IVEG.EQ.3) WRITE(20,956) cg%VEGCD
c       lzhu edits ends

 955    FORMAT('NO vegetation in computation domain'/)
 956    FORMAT('VEGETATION whose density, width, height and root depth
     +    are specified as input'/'Vegetation drag coefficient
     +    VEGCD=',
     +    F5.2/)
 957    FORMAT('VEGETATION whose density,width and height are
     +    specified as input'/'Vegetation drag coefficient VEGCD=',
     +    F5.2/)
C
        WRITE(20,960) cg%GAMMA
 960    FORMAT('BREAKER RATIO PARAMETER'/
     +     'Input Gamma =',F5.2/)
C
        IF(cg%IPROFL.EQ.1) WRITE(20,970) cg%D50*1000.D0,cg%WF,cg%SG,
     +     cg%EFFB,cg%SLP,
     +     cg%TANPHI,cg%BLP
        IF(cg%IPROFL.EQ.1.AND.cg%IOVER.EQ.1) THEN
          WRITE(20,971) cg%SLPOT
          IF(cg%INFILT.EQ.1) WRITE(20,972) cg%WPM
        ENDIF
 970    FORMAT('SEDIMENT PARAMETERS if IPROFL=1'/
     +     'Median diameter (mm)                D50=   ',F8.2/
     +     'Fall velocity (m/s)                  WF=    ',F6.4/
     +     'Specific gravity                     SG=   ',F5.2/
     +     'Suspension efficiency                eB=   ',F6.3/
     +     'Suspended load parameter               =   ',F5.2/
     +     'Tangent(friction ANGLE)                =   ',F5.2/
     +     'Bedload parameter                     b=    ',F6.4)
 971    FORMAT('Susp.load para. (IOVER=1)              =   ',F5.2/)
 972    FORMAT('INFILT=1 and infiltr. velocity (m/s)   =   ',F7.5/)
C
C     ......... INPUT WAVE AND WATER LEVEL
c mg   nout = 10000
       nout = 10
        WRITE(20,1001) cg%NTIME, nout
        NTIM9=nout+1
        IF(cg%NTIME.GT.(2*nout)) NTIM9=cg%NTIME-(nout-1)
        DO 130 I = 1,cg%NTIME
          IF(I.LE.nout.OR.I.GE.NTIM9) THEN
            WRITE(20,1002) cg%TIMEBC(I+1),cg%TPBC(I),cg%HRMSBC(I),
     +          cg%WSETBC(I),cg%SWLBC(I), cg%WANGBC(I)
          ENDIF
 130    CONTINUE
 1001   FORMAT(/'INPUT WAVE AND WATER LEVEL:NTIME=',I6,' from
     +     TIME=0.0'/
     +     'Output lines are limited to first and last',I6,' lines'/
     +     'End TIME(sec) TP (sec)  HRMS(m) Wave Setup(m)',
     +     'Storm tide(m) ANGLE(deg)'/)
 1002   FORMAT(F11.1,5F11.4)
  888  CONTINUE
C     End of L=1 output................................................
C
C     ......... OUTPUT BOTTOM GEOMETRY
C     The bottom geometry is divided into segments of
C     different inclination and roughness starting from
C     seaward boundary for cg%ILINE cross-shore lines.
C     cg%NBINP(L)  = number of input points for L line
C     cg%XBINP(J,L)  = horizontal distance from seaward boundary
C     to landward-end of segment (J-1) in meters
C     cg%ZBINP(J,L)  = dimensional vertical coordinate (+ above datum)
C     of the landward end of segment (J-1) in meters
C     cg%FBINP(J,L) = bottom friction factor
        WRITE (20,1100) L, cg%YLINE(L), cg%AGLINE(L),
     +     0.D0-cg%ZBINP(1,L),
     +     cg%NBINP(L), cg%DX, cg%XS(L), cg%JMAX(L)
C
 1100   FORMAT (/'INPUT BEACH AND STRUCTURE GEOMETRY'/
     +     'Cross-shore line number               L=  ',I3/
     +     'Alongshore coordinate             YLINE=  ',F13.4/
     +     'Line ANGLE(degrees)              AGLINE=  ',F13.4/
     +     'Depth at seaward boundary (m)          =  ',F13.6/
     +     'Number of input points            NBINP=  ',I8/
     +     'Output lines are limited to first and last 5 lines'/
     +     'Node spacing (m)                    DX=  ',F13.6/
     +     'Shoreline location (m) of ZB=0       XS=  ',F13.6/
     +     'Maximum landward node              JMAX=',I8//
     +     'X  (m)      ZB  (m)  Fric.factor  Wire mesh')
        WRITE (20,1200) cg%XBINP(1,L), cg%ZBINP(1,L)
        NBINP4=6
        IF(cg%NBINP(L).GT.10) NBINP4=cg%NBINP(L)-4
        DO 140 J = 2,cg%NBINP(L)
          IF(J.LE.5.OR.J.GE.NBINP4) THEN
            IF(cg%ISEDAV.LE.1) THEN
              WRITE (20,1200) cg%XBINP(J,L), cg%ZBINP(J,L),
     +          cg%FBINP(J-1,L)
            ELSE
              WRITE (20,1202) cg%XBINP(J,L), cg%ZBINP(J,L),
     +          cg%FBINP(J-1,L),
     +           cg%WMINP(J-1,L)
            ENDIF
          ENDIF
 140    CONTINUE
        IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
          IF(cg%ICLAY.EQ.0) THEN
              WRITE(20,1150) L,cg%NPINP(L)
          ELSE
              WRITE(20,1151) L,cg%NPINP(L)
          ENDIF
          NPINP4=6
          IF(cg%NPINP(L).GT.10) NPINP4=cg%NPINP(L)-4
          DO 141 J=1,cg%NPINP(L)
            IF(J.LE.5.OR.J.GE.NPINP4) THEN
              IF(cg%ICLAY.EQ.0) THEN
                  WRITE(20,1201) cg%XPINP(J,L), cg%ZPINP(J,L)
              ELSE
                  WRITE(20,1202) cg%XPINP(J,L),cg%ZPINP(J,L),
     +             cg%RCINP(J,L),
     +             cg%FCINP(J,L)
              ENDIF
            ENDIF
 141      CONTINUE
        ENDIF
C     where the number of the output lines is limited to 10 or less
C     to reduce the length of the output file ODOC.
 1150   FORMAT(/'INPUT IMPERMEABLE HARD BOTTOM GEOMETRY'/
     +     'Number of input points for line L=',I3, ' NPINP= ',I5//
     +     'X  (m)        ZP  (m)  ')
 1151   FORMAT(/'INPUT ERODIBLE CLAY BOTTOM ELEVATION'/
     +     'Number of input points for line L=',I3, 'NPINP= ',I5//
     +     'X(m)      ZP(m)    RC(m*m/s/s),sand frac  ')
 1200   FORMAT(3F10.3)
 1201   FORMAT(2F10.3)
 1202   FORMAT(4F10.3)
C
C.....OUTPUT VEGETATION CHARACTERISTICS FOR cg%IVEG=1 2 or 3
      IF(cg%IVEG.GE.1) THEN
c       lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
        IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
          WRITE(20,1161)
        ELSE
          WRITE(20,1160)
        ENDIF
        DO 135 J=2,cg%NBINP(L)
          IF(J.LE.5.OR.J.GE.NBINP4) THEN
            J1=J-1
c           lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
            IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
              WRITE(20,1203) cg%XBINP(J,L),cg%VEGN(J1,L),cg%VEGB(J1,L),
     +        cg%VEGD(J1,L),cg%VEGRD(J1,L)
            ELSE
              WRITE(20,1202) cg%XBINP(J,L),cg%VEGN(J1,L),cg%VEGB(J1,L),
     +          cg%VEGD(J1,L)
            ENDIF
          ENDIF
 135    CONTINUE
      ENDIF
 1160 FORMAT(/'INPUT VEGETATION CHARACTERISITCS'/
     +   'X (m)   DENSITY(1/m/m) WIDTH(m) HEIGHT(m) ')
 1161 FORMAT(/'INPUT VEGETATION CHARACTERISITCS'/
     +   'X(m)  DEN.(1/m/m) WID.(m) HEI.(m) ROD.(m)')
 1203 FORMAT(5F8.3)
C
C.....OUTPUT DIKE GRASS AND SOIL CHARACTERISTICS FOR cg%IPROFL=2
      IF(cg%IPROFL.EQ.2) THEN
        WRITE(20,1170)
        DO 136 J=2,cg%NBINP(L)
          IF(J.LE.5.OR.J.GE.NBINP4) THEN
            J1=J-1
            WRITE(20,1210) cg%XBINP(J,L),cg%GDINP(J1,L),cg%GRINP(J1,L),
     +         cg%GRDINP(J1,L)
          ENDIF
 136    CONTINUE
      ENDIF
 1170 FORMAT(/'INPUT GRASS AND SOIL CHARACTERISTICS'/
     +   'X (m)   THICKNESS(m)  RO(m*m/s/s)  RD(m*m/s/s)')
 1210 FORMAT(4F11.3)
C
C.....INPUT WIND SHEAR STRESSES FOR cg%IWIND=1
      IF(L.GT.1) GOTO 889
        IF(cg%IWIND.EQ.1) THEN
          WRITE(20,1370)
          DO 142 I=1,cg%NTIME
            IF(I.LE.10.OR.I.GE.NTIM9) THEN
              WRITE(20,1371) cg%TIMEBC(I),cg%TIMEBC(I+1),cg%W10(I),
     +           cg%WANGLE(I),
     +           cg%WINDCD(I)
            ENDIF
 142      CONTINUE
        ENDIF
 1370   FORMAT(/'INPUT WIND SPEED, DIRECTION AND STRESSES'/
     +     'Start & End TIME(s) Speed(m/s) Dir(deg) DragCoef'/)
 1371   FORMAT(2F11.1,2F11.2,E11.4)
C
C.....INPUT LANDWARD STILL WATER LEVEL FOR cg%IWTRAN=1
        IF(cg%IWTRAN.EQ.1) THEN
          IF(cg%ISWLSL.EQ.0) WRITE(20,1380)
          IF(cg%ISWLSL.EQ.1) THEN
            WRITE(20,1381)
            DO 143 I=1,cg%NTIME
              IF(I.LE.10.OR.I.GE.NTIM9) THEN
                WRITE(20,1382) cg%TIMEBC(I),cg%TIMEBC(I+1),cg%SWLAND(I)
              ENDIF
 143        CONTINUE
          ENDIF
          IF(cg%ISWLSL.EQ.2) WRITE(20,1383)
        ENDIF
 1380   FORMAT(/'INPUT LANDWARD STILL WATER LEVEL for IWTRAN=1 and ',
     +     'ISWLSL=0'/'same as input seaward still water level'/)
 1381   FORMAT(/'INPUT LANDWARD STILL WATER LEVEL for IWTRAN=1 and ',
     +     'ISWLSL=1'/'Start & End TIME(s) SWL(m) above datum'/)
 1382   FORMAT(2F11.1,F11.4)
 1383   FORMAT(/'IWTRAN=1 but ISWLSL=2 and NO WATER landward of
     +     crest'/
     +     'Overflow occurs (IOFLOW=1) if crest is submerged'/)
C
C.....INPUT ALONGSHORE WATER LEVEL GRADIENT FOR cg%ITIDE=1
        IF(cg%ITIDE.EQ.1) THEN
          WRITE(20,1390)
          DO 144 I=1,cg%NTIME
            IF(I.LE.10.OR.I.GE.NTIM9) THEN
              WRITE(20,1385) cg%TIMEBC(I),cg%TIMEBC(I+1),cg%DETADY(I)
            ENDIF
 144      CONTINUE
 1385     FORMAT(2F11.1,F11.7)
        ENDIF
 1390   FORMAT(/'INPUT ALONGSHORE WATER LEVEL GRADIENT'/
     +          'Start & End TIME(s)      DETA/DY alongshore'/)
C
C     End of L=1 OUTPUT.....................................................
 889    CONTINUE
      ENDIF
C     --------------------- END OF OUTPUT ONLY WHEN ITIME = 0 --------------
C
C     ------------------- COMPUTED CROSS-SHORE VARIATIONS ------------------
C     For each cross-shore line L of cg%ILINE lines
C     Stored at cg%TIME = 0.0 and at the end of constant wave and
C     water level at the seaward boundary if laboratory data (cg%ILAB=1)
C     For field data (cg%ILAB=0), stored at the beginning, end, and
C     every ten storage cg%TIME levels (GOTO 200 goes to end of this subr.)
C
      IF(ITIME.EQ.0) THEN
        !WRITE(21,1490)L,cg%JMAX(L),cg%TIMEBC(ITIME)
        WRITE(21,1490)L,cg%JMAX(L),cg%TIMEBC(ITIME+1) !BDJ 2021-01-20
        DO 199 J=1,cg%JMAX(L)
          IF(cg%IPERM.EQ.0.AND.cg%ISEDAV.EQ.0) THEN
            WRITE(21,1500)cg%XB(J),cg%ZB(J,L)
          ELSE
            IF(cg%ISEDAV.EQ.1.OR.cg%IPERM.EQ.1) THEN
              WRITE(21,1500) cg%XB(J),cg%ZB(J,L),cg%ZP(J,L)
            ENDIF
            IF(cg%ISEDAV.EQ.2) THEN
              WRITE(21,1500) cg%XB(J),cg%ZB(J,L),cg%ZMESH(J,L),
     +          cg%ZP(J,L)
            ENDIF
          ENDIF
 199  CONTINUE
        GOTO 200
      ENDIF
C
      TIMOUT = cg%TIME
C mg - explicit declaration of laboratory/field data sets
      IF(cg%ILAB.EQ.0) THEN
C
C mg - ensure output at end of simulation for field data sets
        IF(ITIME.EQ.cg%NTIME) GOTO 201
C mg
C        DUM=DBLE(ITIME)/10.D0-DBLE(ITIME/10)
C        IF(DUM.NE.0.D0) GOTO 200
      ENDIF
 201  CONTINUE
      IF(cg%IPROFL.EQ.0) THEN
        TIMOUT = cg%TIMEBC(ITIME+1)
        WRITE(20,1440) TIMOUT,L
      ELSE
        WRITE(20,1441) TIMOUT,L
      ENDIF
 1440 FORMAT(/'**********COMPUTED CROSS-SHORE VARIATIONS**********'/
     +   'on input bottom profile at TIME =',F11.1, '  Line L=',I3/)
 1441 FORMAT(/'**********COMPUTED CROSS-SHORE VARIATIONS**********'/
     +   'on bottom profile computed at TIME (s) = ', F11.1,
     +   '  Line L=',I3/)
C
      WRITE(20,1450) cg%JR, cg%XB(cg%JR), cg%ZB(cg%JR,L), cg%H(cg%JR)
 1450 FORMAT('LANDWARD WET COMPUTATION LIMIT'/
     +   'Most landward node of wet zone computation    JR=',I8/
     +   'X-coordinate at JR (m)                        XR=  '
     +          ,F13.6/
     +   'Bottom elevation at JR (m)                    ZR=  '
     +          ,F13.6/
     +   'Mean water depth at this node (m)          H(JR)=  '
     +          ,F13.6/)
C
C     Wave Reflection Coeffiecient at node J=1 only for cg%IOVER=0
      IF(cg%IOVER.EQ.0) THEN
        IF(cg%JR.GT.cg%JSWL(L).AND.cg%JSWL(L).LT.cg%JMAX(L)) THEN
          DUM = cg%SIGMA(cg%JSWL(L))*cg%SIGMA(cg%JSWL(L))*
     +          cg%CP(cg%JSWL(L))*cg%WN(cg%JSWL(L))
          DUM = DUM/cg%WN(1)/cg%CP(1)
          SIGREF=DSQRT(DUM)
          IF(cg%IANGLE.EQ.1) SIGREF=SIGREF/DSQRT(cg%CTHETA(1)/
     +          cg%CTHETA(cg%JSWL(L)))
          REFCOF=SIGREF/cg%SIGMA(1)
          WRITE(20,1460) REFCOF, cg%JSWL(L)
        ENDIF
      ENDIF
 1460 FORMAT('WAVE REFLECTION COEFFICIENT'/
     +   'Wave reflection coefficient (at x=0) = ',F9.6/
     +   'Still water shoreline node location JSWL =',I5/)
C
C     Output computed wave overtopping, overflow and seepage rates
C     in Subr.10 QORATE
      IF(cg%IOVER.EQ.1)THEN
        IF(cg%IWTRAN.EQ.0.OR.cg%JR.LT.cg%JMAX(L))THEN
          CALL QORATE(cg,ITIME,L,ITEQO,ICONV,1)
        ENDIF
      ENDIF
      IF(cg%JR.EQ.cg%JMAX(L).AND.cg%IWTRAN.EQ.1)THEN
        DUM=cg%SIGMA(cg%JMAX(L))/cg%SIGMA(1)
        WRITE(20,1461) DUM,cg%JMAX(L),cg%RCREST(L),cg%JCREST(L)
 1461   FORMAT('WAVE TRANSMISSION OVER SUBMERGED STRUCTURE'/
     +  'Wave transmission coefficient =',F9.6/
     +  '    at landward end node JMAX=',I5/
     +  'Structure crest elevation (m),RCREST=',F9.4/
     +  '    at crest node JCREST=',I5/)
      ENDIF
C
C     Longshore (Suspended Sediment plus Bedload) Transport Rate
      IF(cg%IPROFL.EQ.1.AND.cg%IANGLE.EQ.1) THEN
        DUM = 0.5D0*(cg%QBY(1)+cg%QSY(1))
        DO 145 J = 2,cg%JDRY-1
          DUM = DUM + (cg%QBY(J)+cg%QSY(J))
 145    CONTINUE
        DUM = DUM + 0.5D0*(cg%QBY(cg%JDRY)+cg%QSY(cg%JDRY))
        QLONG = DUM * cg%DX
        SIGMAX = cg%SIGMA(1)
        JB=1
        DO 150 J=2,cg%JR
          IF(cg%SIGMA(J).GT.SIGMAX) THEN
            SIGMAX = cg%SIGMA(J)
            JB = J
          ENDIF
 150    CONTINUE
        DUM = cg%SIGMA(JB)**2.D0*cg%CP(JB)*cg%WN(JB)*cg%CTHETA(JB)*
     +          cg%STHETA(JB)
        CERCK = (cg%SG-1.D0)*QLONG/DUM
        WRITE(20,1470) QLONG,CERCK,cg%STHETA(JB)
      ENDIF
 1470 FORMAT('LONGSHORE SUSPENDED AND BEDLOAD SAND TRANSPORT RATE'/
     +'Transport Rate (m**3/s) =',F16.8/'CERC Formula K=',F11.3/
     +'sin(breaker ANGLE)=',F11.5/)
C 1470 FORMAT('LONGSHORE SUSPENDED AND BEDLOAD SAND TRANSPORT RATE'/
C     +'Transport Rate (m**3/s) =',E14.5/'CERC Formula K=',F11.3/
C     +'sin(breaker cg%ANGLE)=',F11.5/)
C
C     Damage (normalized eroded area) of stone structure
C     EDMAX = normalized maximum vertical erosion depth
      IF(cg%ISTSAN.EQ.0) THEN
          IF(cg%IPROFL.EQ.1.AND.cg%IPERM.EQ.1) THEN
              EDMAX=0.D0
              DO 300 J=1,cg%JMAX(L)
                  EDEPTH(J)=cg%ZB0(J,L)-cg%ZB(J,L)
                  IF(EDEPTH(J).GT.EDMAX) EDMAX=EDEPTH(J)
                  IF(EDEPTH(J).LT.0.D0) EDEPTH(J)=0.D0
 300      CONTINUE
          EDMAX=EDMAX/cg%SDP
          JMAXL=cg%JMAX(L)
          CALL INTGRL(JMAXL,cg%DX,EDEPTH,AREA)
          DAMAGE=AREA/cg%SDP/cg%SDP
          STABNO=cg%SQR2*cg%HRMS(1)/cg%SDP/(cg%SG-1.D0)
          WRITE(20,1480) DAMAGE,EDMAX,STABNO
          ENDIF
      ENDIF
 1480 FORMAT('STONE STRUCTURE DAMAGE'/
     +'Damage S=',F10.3/ 'Normalized erosion depth E=',F10.3/
     +'Stability number Nmo=',F8.3/)
C
C.........COMPUTED CROSS-SHORE VARIATIONS
C
C     Indicate the number of lines used for storage at specified cg%TIME
C     for each cross-shore line L=1,2,...,cg%ILINE
      JSWASH = cg%JDRY - cg%JWD +1
      JDUM = cg%JR
      IF(cg%IOVER.EQ.1) THEN
        JDUM=cg%JDRY
        IF(cg%IWTRAN.EQ.1.AND.cg%JSL.LT.cg%JMAX(L)) JDUM=cg%JMAX(L)
      ENDIF
      WRITE(22,1490) L,JDUM,TIMOUT
      WRITE(23,1490) L,cg%JR,TIMOUT
      WRITE(24,1490) L,cg%JR,TIMOUT
      WRITE(25,1490) L,cg%JR,TIMOUT
      WRITE(26,1490) L,cg%JR,TIMOUT
      WRITE(27,1490) L,JDUM,TIMOUT
      WRITE(28,1490) L,JDUM,TIMOUT
      WRITE(29,1490) L,cg%JR,TIMOUT
      WRITE(30,1490) L,JDUM,TIMOUT
      WRITE(31,1490) L,cg%JR,TIMOUT
      WRITE(32,1490) L,cg%JMAX(L),TIMOUT
      WRITE(33,1490) L,cg%JMAX(L),TIMOUT
      WRITE(37,1490) L,cg%JMAX(L),TIMOUT
      WRITE(38,1490) L,cg%JMAX(L),TIMOUT
      WRITE(39,1490) L,cg%JMAX(L),TIMOUT
      IF(cg%IOVER.EQ.1) THEN
        WRITE(34,1490)L,JDUM,TIMOUT
        WRITE(35,1490)L,JSWASH,TIMOUT
        TIMID=0.5D0*(cg%TIMEBC(ITIME)+cg%TIMEBC(ITIME+1))
        DUM=cg%TIMEBC(ITIME+1)-cg%TIMEBC(ITIME)
        WRITE(36,1491) L,TIMID,(cg%TSQO(L)/DUM),(cg%TSQBX(L)/DUM),
     +     (cg%TSQSX(L)/DUM)
      ENDIF
 1490 FORMAT(2I8,F11.1)
 1491 FORMAT(I8,4F17.9)
C
      IF(cg%IPROFL.GE.1.AND.L.EQ.cg%ILINE) THEN
        DO 181 LL=1,cg%ILINE
          WRITE(21,1490) LL,cg%JMAX(LL),TIMOUT
          DO 180 J=1,cg%JMAX(LL)
            IF(cg%IPERM.EQ.0.AND.cg%ISEDAV.EQ.0) THEN
c             lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
              IF (cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
                WRITE(21,1500) cg%XB(J),cg%ZB(J,LL),cg%UPROOT(J,LL)
              ELSE
                WRITE(21,1500) cg%XB(J),cg%ZB(J,LL)
              ENDIF
            ELSE
c             lzhu change to cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3
              IF(cg%IVEG.EQ.1.OR.cg%IVEG.EQ.3) THEN
                WRITE(21,1500) cg%XB(J),cg%ZB(J,LL),cg%ZP(J,LL),
     +                         cg%UPROOT(J,LL)
              ELSE
                IF(cg%ISEDAV.EQ.1.OR.cg%IPERM.EQ.1) THEN
                  IF(cg%ISTSAN.EQ.0) WRITE(21,1500) cg%XB(J),
     +                          cg%ZB(J,LL),cg%ZP(J,LL)
                ENDIF
                IF(cg%ISEDAV.EQ.2) THEN
                  WRITE(21,1500) cg%XB(J),cg%ZB(J,LL),cg%ZMESH(J,LL),
     +                           cg%ZP(J,LL)
                ENDIF
                IF(cg%ISTSAN.EQ.1) THEN
                  WRITE(21,1500) cg%XB(J),cg%ZB(J,LL),cg%ZP(J,LL),
     +                           cg%VDSAND(J)
                ENDIF
              ENDIF
            ENDIF
 180      CONTINUE
 181    CONTINUE
      ENDIF
C
C     Smooth computed cg%PB(J), cg%VS(J), and cg%PS(J) before storing and plotting
      IF(cg%IPROFL.EQ.1) THEN
        DUMVEC=cg%PB
        CALL SMOOTH(cg,JDUM,DUMVEC,cg%PB)
        DUMVEC=cg%VS
        CALL SMOOTH(cg,JDUM,DUMVEC,cg%VS)
        DUMVEC=cg%PS
        CALL SMOOTH(cg,JDUM,DUMVEC,cg%PS)
      ENDIF
C
      DO 160 J = 1,cg%JR
        WRITE(22,1500) cg%XB(J),(cg%H(J)+cg%ZB(J,L)),cg%H(J),
     +                           cg%SIGMA(J)
        WRITE(23,1500) cg%XB(J),cg%WT(J),cg%QBREAK(J),cg%SIGSTA(J)
        WRITE(24,1500) cg%XB(J),cg%SXXSTA(J),cg%TBXSTA(J)
        IF(cg%IANGLE.EQ.1) WRITE(25,1500) cg%XB(J),cg%SXYSTA(J),
     +                           cg%TBYSTA(J)
        WRITE(26,1500) cg%XB(J),cg%EFSTA(J)/cg%WT(J),cg%DBSTA(J),
     +                           cg%DFSTA(J)
        IF(cg%IPERM.EQ.0) THEN
          WRITE(27,1500) cg%XB(J),cg%UMEAN(J),cg%USTD(J)
        ELSE
          WRITE(27,1500) cg%XB(J),cg%UMEAN(J),cg%USTD(J),cg%UPMEAN(J)
        ENDIF
        IF(cg%IANGLE.EQ.1) WRITE(28,1500) cg%XB(J),cg%STHETA(J),
     +                           cg%VMEAN(J),cg%VSTD(J)
        IF(cg%IROLL.EQ.1) WRITE(29,1500) cg%XB(J),cg%RQ(J)
        IF(cg%IPROFL.EQ.1) WRITE(30,1500) cg%XB(J),cg%PB(J),cg%PS(J),
     +                                    cg%VS(J)
        IF(cg%IPERM.EQ.1) WRITE(31,1500) cg%XB(J),cg%UPSTD(J),
     +                                   cg%DPSTA(J)
 160  CONTINUE
      IF(cg%IOVER.EQ.1) THEN
C     Store mean values over wet duration
        IF(cg%JDRY.GE.cg%JR.AND.cg%IOVER.EQ.1) THEN
        DO 170 J=(cg%JR+1),JDUM
          DUM=cg%H(J)+cg%ZB(J,L)
          IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
            IF(cg%JX2.LT.cg%JMAX(L)) THEN
              IF(cg%JXW.LE.J.AND.J.LE.cg%JX2) THEN
                DUM=cg%H(J)+cg%ZW
                cg%PWET(J)=1.D0
              ENDIF
            ENDIF
          ENDIF
          WRITE(22,1500) cg%XB(J),DUM,cg%H(J),cg%SIGMA(J)
          IF(cg%IPERM.EQ.0) THEN
            WRITE(27,1500) cg%XB(J),cg%UMEAN(J),cg%USTD(J)
          ELSE
            WRITE(27,1500) cg%XB(J),cg%UMEAN(J),cg%USTD(J),cg%UPMEAN(J)
          ENDIF
          IF(cg%IANGLE.EQ.1) WRITE(28,1500) cg%XB(J),cg%STHETA(J),
     +        cg%VMEAN(J),cg%VSTD(J)
          IF(cg%IPROFL.EQ.1) WRITE(30,1500) cg%XB(J),cg%PB(J),cg%PS(J),
     +                                      cg%VS(J)
 170    CONTINUE
        ENDIF
C     Where cg%UPMEAN, cg%PB, cg%PS, cg%VS, and cg%QP include effects of cg%PWET.
        DO 171 J=1,JDUM
          IF(cg%IPERM.EQ.0) WRITE(34,1500) cg%XB(J),cg%PWET(J)
          IF(cg%IPERM.EQ.1) WRITE(34,1500) cg%XB(J),cg%PWET(J),cg%QP(J)
 171    CONTINUE
        DO 161 J=cg%JWD,cg%JDRY
          WRITE(35,1500) cg%XB(J),cg%HEWD(J),cg%UEWD(J),cg%QEWD(J)
 161    CONTINUE
      ENDIF
C
      IF(cg%IPROFL.EQ.1) THEN
C     Smooth computed cg%QBX(J) and cg%QSX(J) before storing and plotting
        JMAXL=cg%JMAX(L)
        DUMVEC = cg%QBX
        CALL SMOOTH(cg,JMAXL,DUMVEC,cg%QBX)
        DUMVEC = cg%QSX
        CALL SMOOTH(cg,JMAXL,DUMVEC,cg%QSX)
C     Smooth computed cg%QBY(J) and cg%QSY(J) if cg%IANGLE=1
        IF(cg%IANGLE.EQ.1) THEN
          DUMVEC=cg%QBY
          CALL SMOOTH(cg,JMAXL,DUMVEC,cg%QBY)
          DUMVEC=cg%QSY
          CALL SMOOTH(cg,JMAXL,DUMVEC,cg%QSY)
        ENDIF
        DO 162 J=1,cg%JMAX(L)
          WRITE(32,1500) cg%XB(J),cg%QBX(J),cg%QSX(J),
     +                   (cg%QBX(J)+cg%QSX(J))
          IF(cg%IANGLE.EQ.1) WRITE(33,1500) cg%XB(J),cg%QBY(J),
     +       cg%QSY(J),
     +       (cg%QBY(J) + cg%QSY(J))
 162    CONTINUE
C     Store sediment transport volume per unit width
C     during cg%TIME=0.0 to cg%TIME=TIMOUT
        DO 163 J=1,cg%JMAX(L)
          WRITE(37,1500) cg%XB(J),cg%VBX(J,L),cg%VSX(J,L),
     +                   (cg%VBX(J,L)+cg%VSX(J,L))
          IF(cg%IANGLE.EQ.1) WRITE(38,1500) cg%XB(J),cg%VBY(J,L),
     +        cg%VSY(J,L),
     +        (cg%VBY(J,L)+cg%VSY(J,L))
 163    CONTINUE
      ENDIF
C
C     If cg%IPROFL=2 or cg%ICLAY=1, the following variables related to dike
C     erosion at node J and line L are computed in Subr.22 EROSON
C     cg%EDIKE(J,L)=downward erosion depth (m) from initial (cg%TIME=0.0)
C     dike surface at cg%TIME=TIMOUT for cg%IPROFL=2 and cg%ICLAY=0
C     cg%EPCLAY(J)=downward clay erosion depth (m) for cg%ICLAY=1 and cg%IPROFL=1
C     cg%DSTA(J)=variable (m*m/s) related to energy dissipation and dike or clay
C     erosion forcing at cg%TIME=TIMOUT
C     cg%DSUM(J)= cumulative forcing (m*m) obtained by integrating cg%DSTA(J)
C     from cg%TIME=0.0 to cg%TIME=TIMOUT
      IF(cg%IPROFL.EQ.2.OR.cg%ICLAY.EQ.1) THEN
        DO 164 J=1,cg%JMAX(L)
          IF(cg%IPROFL.EQ.2) THEN
              WRITE(39,1500) cg%XB(J),cg%EDIKE(J,L),cg%DSTA(J),
     +                       cg%DSUM(J)
          ELSE
              WRITE(39,1500) cg%XB(J),cg%EPCLAY(J,L),cg%DSTA(J)
          ENDIF
 164    CONTINUE
      ENDIF
C
 1500 FORMAT(4F17.9)
C
 200  CONTINUE
      RETURN
      END SUBROUTINE OUTPUT
C
C     -08-----------------  END OF SUBROUTINE OUTPUT  ---------------------

C     #13##################### SUBROUTINE INTGRL ##########################
      SUBROUTINE INTGRL(NUM,DEL,F,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NN=5000)
      DIMENSION F(NN)
C
C     NUM can be even or odd integer
      IMAX = (NUM-1)/2
      DUM = DBLE(NUM-1)/2.D0
      IF(DBLE(IMAX).LT.DUM) THEN
        NEND = NUM - 1
        NEVEN = 1
      ELSE
        NEND = NUM
      ENDIF
      SE = F(2)
      SO = 0.D0
      DO 500 I=2,IMAX
        SE = SE + F(I*2)
        SO = SO + F(I*2-1)
 500  CONTINUE
      G = DEL/3.D0*(F(1) + 4.D0*SE + 2.D0*SO + F(NEND))
      IF(NEVEN.EQ.1) G=G+(F(NEND)+F(NUM))*DEL/2.D0
      RETURN
      END SUBROUTINE INTGRL
C     -13-----------------  END OF SUBROUTINE INTGRL  ---------------------


C     #10#####################  SUBROUTINE QORATE  ########################
C
C     This subroutine computes overtopping, overflow and seepage rates
C
      SUBROUTINE QORATE(cg,ITIME,L,ITEQO,ICONV,ICALL)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
C
      PARAMETER (NN=5000, NB=30000,NL=100)

      DIMENSION   WSET(NN),ZRW(NN),SIGPT(NN)
C
C
C     Find overtopping, overflow and seepage rates during ICALL=0
C     ...................... Start of ICALL=0 ...........................
      IF(ICALL.EQ.0) THEN
C
C     Predict combined wave overtopping and overflow rate cg%QOTF
C     by calling Subr.16 WETDRY for wet and dry zone
        CALL WETDRY(cg,ITIME,L,ITEQO)
C
C     Compute new combined rate QONEW and check convergency (one percent)
C     Allowable error is increased for QONEW less than 1.D-2(m*m/s)
        QONEW = cg%QOTF
        IF(cg%IPERM.EQ.1) QONEW=QONEW+cg%SPRATE
        IF(QONEW.LT.1.D-5) THEN
C         IF(cg%QO(L).LT.1.D-5) THEN
            ICONV = 0
            cg%QO(L)=QONEW
            GOTO 99
C         ELSE
C           GOTO 98
C         ENDIF
        ENDIF
        DUM = DABS(QONEW-cg%QO(L))/QONEW
        AER=1.D-4/QONEW
        IF(AER.LT.1.D-2) AER=1.D-2
        IF(DUM.LT.AER) THEN
          ICONV = 0
          cg%QO(L)=QONEW
          GOTO 99
        ENDIF
 98     ICONV = 1
C     To avoid numerical oscillation and accelerate convergance
C     use FRACTN of previous value and (1.0-FRACTN) of new value
        FRACTN = 0.5D0 + 0.1D0*ITEQO
        IF(FRACTN.GT.0.9D0) FRACTN=0.9D0
        IF(ITEQO.EQ.10) FRACTN=0.5D0
        IF(ITEQO.EQ.20) FRACTN=0.D0
        cg%QO(L) = FRACTN*cg%QO(L) + (1.D0-FRACTN)*QONEW
        IF(cg%IWTRAN.EQ.1) THEN
          DUM=cg%GRAV*cg%SIGMA(1)*cg%SIGMA(1)*cg%CTHETA(1)/cg%CP(1)
          IF(cg%QO(L).GT.DUM) cg%QO(L)=DUM
        ENDIF
C
 99     CONTINUE
        IF(cg%IPOND.EQ.1) THEN
          IF(cg%NOPOND.EQ.1) cg%QM=cg%QO(L)
          IF(cg%JCREST(L).EQ.cg%JXW) cg%QM=cg%QO(L)
          IF(cg%ZW.GE.cg%ZB(cg%JMAX(L),L)) cg%QM=cg%QO(L)
        ENDIF
      ENDIF
C.....................................End of ICALL=0........................
C
C....................................Start of ICALL=1.......................
C     Output computed values in file 20 'ODOC' if ICALL=1 in Subr.8 OUTPUT
      IF(ICALL.EQ.1) THEN
C
C     Mean (ERMEAN) above datum Z=0 and standard deviation (SIGRUN) of runup
C     WSET(J)=wave setup above datum Z=0.0 during wet+dry duration
C     SIGPT(J)=standard deviation during wet+dry duration
C     ZRW(J)=runup wire elevation (cg%RWH above bottom) at node J above Z=0.0
C
        DO 170 J=1,cg%JCREST(L)
          IF(J.LE.cg%JDRY) THEN
            WSET(J)= cg%H(J)*cg%PWET(J)+cg%ZB(J,L)
          ELSE
            WSET(J)=cg%ZB(J,L)
          ENDIF
          SIGPT(J)=cg%SIGMA(J)*cg%PWET(J)
          ZRW(J)=cg%ZB(J,L)+cg%RWH
 170    CONTINUE
C
C     K=1,2 and 3 correspond to intersections of ZRW with WSET, (WSET-SIGPT)
C     and (WSET+SIGPT), respectively
        DO 100 K=1,3
          J=cg%JDRY
          IF(cg%JDRY.GT.cg%JCREST(L)) J=cg%JCREST(L)
          DUM1=ZRW(J)-WSET(J)
          IF(K.EQ.2) DUM1=DUM1+SIGPT(J)
          IF(K.EQ.3) DUM1=DUM1-SIGPT(J)
          IF(DUM1.LT.0.D0) THEN
            IF(K.EQ.1) THEN
              ETARUN=WSET(J)
              GOTO 100
            ENDIF
            IF(K.EQ.2) THEN
              Z1RUN=WSET(J)-SIGPT(J)
              X1RUN=cg%XB(J)  !bdj
              GOTO 100
            ENDIF
            IF(K.EQ.3) THEN
              Z2RUN=WSET(J)+SIGPT(J)
              X2RUN=cg%XB(J)  !bdj
              IF(X2RUN.LE.X1RUN)X2RUN=X1RUN+cg%DX  !bdj
              GOTO 100
            ENDIF
          ENDIF
 105      J=J-1
          DUM2=ZRW(J)-WSET(J)
          IF(K.EQ.2) DUM2=DUM2+SIGPT(J)
          IF(K.EQ.3) DUM2=DUM2-SIGPT(J)
          IF(DUM2.GT.0.D0) THEN
            DUM1=DUM2
            GOTO 105
          ELSE
            DUM3=DUM1-DUM2
            DUMJ1=-DUM2/DUM3
            DUMJ=DUM1/DUM3
            DUMETA=DUMJ*WSET(J)+DUMJ1*WSET(J+1)
            IF(K.EQ.1) ETARUN=DUMETA
            IF(K.EQ.2) THEN
              Z1RUN=DUMETA-DUMJ*SIGPT(J)-DUMJ1*SIGPT(J+1)
              X1RUN=DUMJ*cg%XB(J)+DUMJ1*cg%XB(J+1)
            ENDIF
            IF(K.EQ.3) THEN
              Z2RUN=DUMETA+DUMJ*SIGPT(J)+DUMJ1*SIGPT(J+1)
              X2RUN=DUMJ*cg%XB(J)+DUMJ1*cg%XB(J+1)
C BDJ 2011->2014 on 2014-10-02
              IF((WSET(J+1)-WSET(J))/SIGPT(J).GT.10.*cg%DX) THEN
                DUMETA=WSET(J)  !bdj
                Z2RUN=DUMETA+SIGPT(J) !bdj
                X2RUN=cg%XB(J)     !bdj
                IF(x2run-x1run.le.01D0*cg%DX) THEN
                  Z2RUN=Z1RUN  + .01D0*cg%DX*cg%BSLOPE(J,L)
                  X2RUN=X1RUN + .01D0*cg%DX
                ENDIF
              ENDIF
C end BDJ 2011->2014 on 2014-10-02
            ENDIF
          ENDIF
 100    CONTINUE
        SIGRUN=(Z2RUN-Z1RUN)/2.D0
        ERMEAN=(Z1RUN+ETARUN+Z2RUN)/3.D0
        SLPRUN=(Z2RUN-Z1RUN)/(X2RUN-X1RUN)

C bdj 2015-03-11  added catch for negative slopes
        SIGRUN=max(0.D0,SIGRUN)
        ERMEAN=max(z1run,ERMEAN)
        SLPRUN=max(0.D0,SLPRUN)
C end bdj 2015-03-11 added catch for negative slopes
C bdj 2015-07-06  added catch for cases where waves are very small
        IF(cg%JR.LT.NINT(cg%JSWL(L)/2.)) THEN
           SIGRUN=0.D0
           ERMEAN=cg%SWLBC(ITIME)
           SLPRUN=0.D0
        ENDIF
C end bdj 2015-07-06  added catch for cases where waves are very small
C
C     R13=significant runup height above Z=0.0
C     R2P=two percent runup height above Z=0.0
C     RKAPPA=Kappa for runup probability distribution
        IF(cg%IPERM.EQ.1) THEN
          R13=ERMEAN+(2.D0+SLPRUN)*SIGRUN
          RSC=(cg%RCREST(L)-ERMEAN)/(R13-ERMEAN)
          RKAPPA=2.0D0+0.5D0/RSC**3.D0
        ELSE
          DUM=4.D0*SLPRUN
C BDJ 2011->2014 on 2014-10-02
C          IF(DUM.GT.2.D0) DUM=2.D0
          IF(DUM.GT.1.D0) DUM=1.D0
C end BDJ 2011->2014 on 2014-10-02
          R13=(ERMEAN-cg%SWLBC(ITIME)+2.D0*SIGRUN)*(1.D0+DUM)+
     +        cg%SWLBC(ITIME)
          RKAPPA=2.0D0
        ENDIF

        IF(cg%RCREST(L).GT.ERMEAN) THEN
          R2P=ERMEAN+(R13-ERMEAN)*1.4D0**(2.D0/RKAPPA)
          R1P=ERMEAN+(R13-ERMEAN)*1.52D0**(2.D0/RKAPPA)
c lzhu change here on April 23, 2018
c The runup code is still under developing
c          WRITE(*,*) 'Rayleigh is on with coeff=',1.4D0**(2.D0/RKAPPA)
c          IF (cg%IWEIBULL.EQ.1) THEN
c             WRITE(*,*) 'Weibull is on'
c             DUMC = DSQRT(DLOG(50.0D0))/cg%RUNUPPHI
c             DUMK = DUMC / DSQRT(2.0D0) / (1+cg%RUNUPKAPPA*DUMC)
c             WRITE(*,*) 'DUMK=', DUMK
c             R2P=ERMEAN+(R13-ERMEAN)*DUMK
c          ENDIF
c end of lzhu changes
        ELSE
          RKAPPA=1000.D0
          R2P=R13
        ENDIF
C
C.....Output swash hydrodynamics computed in Subr.16 WETDRY..........
        IF(cg%JDRY.GE.cg%JCREST(L)) THEN
          POTF=(DTANH(5.D0*cg%PWET(cg%JCREST(L))))**0.8D0
        ELSE
          POTF=0.D0
        ENDIF
C     Depth cg%H, velocity U and discharge Q corresponding to exceedance
C     probability cg%EWD specified in Subr.04 PARAM
        IF(cg%JWD.LE.cg%JDRY) THEN
          DO 300 J=cg%JWD, cg%JDRY
            DUM = cg%PWET(J)/cg%EWD
            IF(DUM.LT.1.1D0) DUM=1.1D0
            cg%HEWD(J)=(cg%H(J)/cg%PWET(J))*DLOG(DUM)
            DUM=cg%USWD(J)
            IF(DUM.LT.0.D0) DUM=0.D0
            cg%UEWD(J) = cg%AWD*DSQRT(cg%GRAV*cg%HEWD(J))+DUM
            cg%QEWD(J) = cg%HEWD(J)*cg%UEWD(J)
 300      CONTINUE
        ENDIF
C     Where computed cg%HEWD(J), cg%UEWD(J) and cg%QEWD(J) are stored in Subr.8 OUTPUT
C
        if (cg%enable_cshore_outputfiles) then
        WRITE(20,920) cg%SWLBC(ITIME),L,cg%RCREST(L),cg%JSWL(L),
     +     cg%JWD,cg%H1,cg%JDRY,POTF,
     +     (cg%QO(L)-cg%SPRATE), cg%SPRATE, cg%QO(L), ITEQO
        endif
 920    FORMAT('COMBINED WAVE OVERTOPPING AND OVERFLOW'/
     +     'Still water level above Z=0 (m)              SWL=  ',F13.6/
     +     'Cross-shore line number                        L=  ',I3/
     +     'Structure or dune creat elevation (m)     RCREST=  ',
     +        F13.6/
     +     'Node number at SWL                          JSWL=  ',I6/
     +     'Wet and dry transition node                  JWD=  ',I6/
     +     'Mean water depth H1(m) at node JWD         H1=  ',
     +        F13.6/
     +     'End node of wet and dry zone                JDRY=  ',I6/
     +     'Wave overtopping probability at JCREST      POTF=  ',
     +        F13.6/
     +     'Comb. overtopping and overflow rate(m*m/s)  QOTF=  ',
     +        F13.9/
     +     'Seepage rate(m*m/s) at JCREST                 QP=  ',
     +        F13.9/
     +     'Total rate (QOTF+QP)(m*m/s)                     =  ',
     +        F13.9/
     +     'QO iteration number                       ITEQO=  ',I3/)
C
C.........................Output empirical runup.......................
        if (cg%enable_cshore_outputfiles) then
        WRITE(20,900) L,SLPRUN,ERMEAN,SIGRUN,R13,R2P,R1P
        endif
C
 900    FORMAT('EMPIRICAL WAVE RUNUP'/
     +     'Cross-shore line number                        L=  ',I3/
     +     'Swash zone bottome slope                  SLPRUN=  ',F13.6/
     +     'Mean runup elevation above Z=0 (m)        ERMEAN=  ',F13.6/
     +     'Runup standard deviation (m)              SIGRUN=  ',F13.6/
     +     'Significant runup height above Z=0 (m)       R13=  ',F13.6/
     +     '2 percent runup height above Z=0 (m)         R2P=  ',F13.6/
     +     '1 Percent runup height above z=0 (m)        R1P=  ',F13.6/)
C
        IF(cg%IWTRAN.EQ.1) THEN
C         IF(cg%JDRY.EQ.cg%JSL1.AND.cg%JSL.LT.cg%JMAX(L)) THEN
          if (cg%enable_cshore_outputfiles) then
           WRITE(20,940)L,cg%JSL,cg%XB(cg%JSL),cg%WSETUP(cg%JSL),
     +        cg%SIGMA(cg%JSL),cg%XB(cg%JMAX(L))
     +        ,cg%WSETUP(cg%JMAX(L)),cg%SIGMA(cg%JMAX(L)),
     +        (cg%SIGMA(cg%JMAX(L))/cg%SIGMA(1))
          endif
C         ELSE
C           WRITE(20,941) cg%JDRY,cg%JSL,cg%JSL1
C         ENDIF
        ENDIF
 940    FORMAT('WAVE TRANSMISSION DUE TO OVERTOPPING'/
     +     'Cross-shore line number                        L=  ',I3/
     +     'Starting node for wave transmission          JSL=  ',I6/
     +     'X-coordinate (m) at node JSL                  XB=  ',
     +        F13.6/
     +     'Wave setup (m) above SWL at node JSL      WSETUP=  ',
     +        F13.6/
     +     'Standard deviation (m) at node JSL         SIGMA=  ',
     +        F13.6/
     +     'X-coordinate (m) at landward end node JMAX      =  ',
     +        F13.6/
     +     'Wave setup (m) above SWL at landward end node JMAX= ',
     +        F13.6/
     +     'Standard dev. (m) at landward end node JMAX     =  ',
     +        F13.6/
     +     'Wave transmission coefficient at JMAX           =  ',
     +        F13.6/)
 941    FORMAT(/'IWTRAN=1 BUT NO WAVE TRANSMISSION'/'JDRY=',I6,
     +     '  and JSL=',I6,'  and JSL1=',I6/'
     +        because entire structure',
     +     ' is submerged or no wet zone exists landward of crest'/)
C
        IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
          if (cg%enable_cshore_outputfiles) then
          WRITE(20,960) L,cg%JCREST(L),cg%JXW,cg%JX2,cg%ZW,cg%QD,cg%QM
          endif
        ENDIF
 960    FORMAT('PONDED WATER IN RUNNEL'/
     +     'Cross-shore line number                        L=  ',I3/
     +     'Ridge crest node                          JCREST=  ',I6/
     +     'Ponded water nodes from                      JXW=  ',I6/
     +     '                     to                      JX2=  ',I6/
     +     'Ponded water level (m)		                ZW=  ',F13.6/
     +     'Wave-induced volume flux (m*m/s)              QD=  ',
     +        F13.6/
     +     'Wave overtopping rate (m*m/s) at JMAX         QM=  ',
     +        F13.6/)
C
C.................................End of ICALL=1...........................
C
      ENDIF
      RETURN
      END SUBROUTINE QORATE
C     -10-----------------  END OF SUBROUTINE QORATE  ---------------------



C     #16####################### SUBROUTINE WETDRY #########################
C     Compute swash hydrodynamics in wet/dry zone (possibly multiple bottom
C     peaks) and combined wave overtopping and overflow rate QOTF.
C
      SUBROUTINE WETDRY(cg,ITIME,L,ITEQO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NB=30000,NL=100)
      DIMENSION G(NN), DG(NN), ETA(NN),ETAP(NN)
C
C     Compute swash variables for node J=cg%JWD to cg%JDRY
C     cg%JWD= wet and dry transition node
C     cg%JR= landward limit of wet computation in Main Program
C     cg%JDRY= landward limit of wet and dry zone
C     cg%PWET(J)= wet probability at node J where cg%PWET(cg%JWD)=1.0
C     cg%HWD(J)= mean water depth where cg%HWD(cg%JWD)=cg%H1
C     cg%USWD(J)= steady swash velocity component
C     cg%UMEAWD(J)= mean velocity in wet and dry zone
C     cg%QP(J)= water flux in permeable layer
C     If cg%INFILT=1, cg%QP(J)=infiltration rate between dune crest and
C     landward node J where cg%QP(J)=0.0 assumed seaward of dune crest
C     cg%UPMWD(J)= mean discharge velocity in permeable layer
      IF(ITEQO.LE.2) THEN
        cg%JWD = cg%JSWL(L)
        IF(cg%IWTRAN.EQ.1.AND.cg%IOFLOW.EQ.1) cg%JWD=cg%JCREST(L)
C     If cg%IPROFL=1 and cg%JWD=cg%JR, no wet and dry zone above SWL
C     shoreline
C     To avoid possible overwash under small waves, no marching
        IF(cg%IPROFL.EQ.1.AND.cg%JWD.GT.cg%JR) THEN
          IF(cg%IPOND.EQ.0) THEN
            cg%JWD=cg%JR
            cg%JDRY=cg%JR
            cg%H1=cg%H(cg%JR)
            GOTO 110
          ENDIF
        ENDIF
        IF(cg%JWD.GT.cg%JR) cg%JWD=cg%JR
        cg%H1 = cg%H(cg%JWD)
      ENDIF
      cg%HWD(cg%JWD) = cg%H1
      BGH3=cg%BWD*cg%GRAV*cg%H1*cg%H1*cg%H1
C BDJ 2011->2014 on 2014-10-02
      SSP_50 = 1.D0 ! The value of cg%SSP such that the CORRECT
!     is .5*(1+cg%GAMMA/cg%SQR8)
      A = 1.D0      ! Dictates the steepness of blending curve near SSP_50
      CORRECT = cg%GAMMA/cg%SQR8
      CORRECT = 0.5D0*(1.D0+CORRECT)+
     +     0.5D0*(1.D0-CORRECT)*tanh(a*(cg%SSP-SSP_50));
!      CORRECT = 1.D0  ! comment this line out to use correction
      cg%SIGWD(cg%JWD) = CORRECT*cg%H1
!      cg%SIGWD(cg%JWD) = cg%H1
C end C BDJ 2011->2014 on 2014-10-02
      PMG1=cg%AWD/DSQRT(cg%GRAV)
      PMGH1=PMG1/cg%H1

      cg%PWET(cg%JWD) = 1.D0
      IF(cg%IPERM.EQ.0) THEN
        cg%QWX=cg%QO(L)
        IF(cg%INFILT.EQ.1) cg%QP(cg%JWD)=0.D0
      ELSE
        cg%QWX=cg%QO(L)-cg%QP(cg%JWD)
        cg%UPMWD(cg%JWD)=cg%UPMEAN(cg%JWD)
        ETA(cg%JWD)=cg%H1+cg%ZB(cg%JWD,L)
        ETAP(cg%JWD)=cg%ZB(cg%JWD,L)
      ENDIF
      QS = cg%QWX-cg%AQWD*cg%H1*DSQRT(cg%GRAV*cg%H1)
      IF(QS.GT.0.D0) QS=0.D0
      cg%USWD(cg%JWD) = QS/cg%H1
      cg%UMEAWD(cg%JWD) = cg%AUWD*DSQRT(cg%GRAV*cg%H1)+cg%USWD(cg%JWD)
      DUM=cg%AGWD*cg%GRAV*cg%H1 - (cg%UMEAWD(cg%JWD)-cg%USWD(cg%JWD))
     +    **2.D0
      IF(DUM.LT.0.D0) THEN
        cg%JDRY=cg%JWD
        GOTO 110
      ENDIF
      cg%USTDWD(cg%JWD)=DSQRT(DUM)
      A = cg%QWX*cg%QWX/BGH3
      A1=A
C
C     Empirical formula for wet probability parameter n=cg%WDN
      cg%WDN=1.01D0+0.98D0*DTANH(cg%QO(L)*cg%QO(L)/BGH3)**0.3D0
      W1=cg%WDN-1.D0
      BNWD=cg%BWD*(1.D0+A1)*(2.D0-cg%WDN)/(cg%WDN-1.D0)
C
C----------------LANDWARD MARCHING COMPUTATION ---------------
C     If cg%IWTRAN=1, the landward wet zone starts from node J=cg%JSL
      JEND=cg%JMAX(L)-1
      IF(cg%IWTRAN.EQ.1.AND.cg%JSL.LT.cg%JMAX(L)) THEN
        JEND=cg%JSL1-1
      ENDIF
C     LSTART=1 indicates beginning of upslope computation
      LSTART=1
      IF(cg%JWD.GT.JEND) THEN
        cg%JDRY=cg%JR
        GOTO 110
      ENDIF
      DO 100 J=cg%JWD,JEND
        JP1 = J+1
C
C------------------ BOTTOM ELEVATION INCREASING LANDWARD -------------------
C     On the seaward upslope and crest(J<cg%JCREST but J<cg%JMAX if cg%IPOND=1)
C     use an empirical formula for wet probability cg%PWET and
C     compute mean depth and return flow velocity cg%USWD for IUPSLP=1
        IUPSLP=0
        JDUM=cg%JCREST(L)
        IF(cg%IPOND.EQ.1) JDUM=cg%JMAX(L)
        IF(J.LT.JDUM.AND.cg%ZB(JP1,L).GE.cg%ZB(J,L)) IUPSLP=1
C     For J=cg%JWD, IUPSLP=1 for any slope
        IF(J.EQ.cg%JWD) IUPSLP=1
C     If cg%IPOND=1 and cg%NOPOND=0, ponded water zone is treated like
C     downslope zone with IUPSLP=0
        IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
          IF(JP1.GE.cg%JXW.AND.JP1.LE.cg%JX2) IUPSLP=0
        ENDIF
        IF(IUPSLP.EQ.1) THEN
          IF(LSTART.EQ.1) THEN
            H2=cg%HWD(J)
            BN12=BNWD*(cg%H1/H2)**W1
            D = BN12 - cg%ZB(J,L)/cg%H1
            AH = cg%AGWD/cg%H1
            G(J) = 0.D0
            DUM=cg%QWX-QS
            IF(DABS(DUM).LT.1.D-3) THEN
              R=0.D0
            ELSE
              R=cg%CWD*QS/DUM
            ENDIF
            IF(cg%IVEG.EQ.0)THEN
              DG(J) = AH*cg%DX*cg%FB2(J,L)*GBWD(R)
C BDJ 2011->2014 on 2014-10-02
C  added to kill friction in wetdry
            DG(J) = 0.D0
C end BDJ 2011->2014 on 2014-10-02

            ELSE
              DUM=cg%HWD(J)/cg%PWET(J)
              X=cg%VEGH(J,L)/DUM
              DG(J)=AH*cg%DX*(cg%FB2(J,L)*GBWD(R)+cg%VEGINP(J,L)*
     +              DUM*GDWD(R,X))
            ENDIF
C     Functions GBWD(R) and GDWD(R,X) are specified below this subroutine
            LSTART=0
          ENDIF
          CX = D + cg%ZB(JP1,L)/cg%H1
          IF(cg%IPERM.EQ.0) THEN
            WPGH=0.D0
          ELSE
            IF(cg%HP(JP1,L).LE.0.D0) THEN
              WPGH=0.D0
            ELSE
              DUM=0.5D0*(cg%HP(J,L)+cg%HP(JP1,L))/cg%SDP
C             IF(DUM.GT.10.D0) DUM=10.D0
              PMGH=PMGH1*DUM**0.3D0
              WPGH=PMGH*cg%WPM*cg%PWET(J)*cg%DX/DSQRT(cg%HWD(J))
            ENDIF
          ENDIF
          DGJP1 = DG(J)+WPGH
          DO 200 ITEH=1,20
            G(JP1) = G(J) + DGJP1
            C = CX + G(JP1)
            IF(C.LE.0.D0) THEN
              cg%JDRY = J
              GOTO 110
            ELSE
              Y = (C/BN12)**(1.D0/W1)
            ENDIF
            cg%HWD(JP1)=H2/Y
            if (cg%HWD(jp1).gt.cg%HWD(jp1-1)) cg%HWD(jp1) =
     +         cg%HWD(jp1-1) !bdj 2021-01-21
            Y=cg%H1/cg%HWD(JP1)
            DUM = (1.D0 + A1)*Y**cg%WDN - A*Y*Y*Y
C           IF(DUM.LE.0.D0) THEN
C             cg%JDRY=J
C             GOTO 110
C           ENDIF
            IF(DUM.LT.1.D0) THEN
              cg%PWET(JP1) = cg%PWET(J)
            ELSE
              cg%PWET(JP1) = 1.D0/DUM
              IF(cg%PWET(JP1).GT.cg%PWET(J)) cg%PWET(JP1)=cg%PWET(J)
            ENDIF
            QWAVE=cg%AQWD*cg%HWD(JP1)*DSQRT(cg%GRAV*cg%HWD(JP1)/
     +            cg%PWET(JP1))
C
C     Compute cg%QP and cg%UPMWD in permeable layer if cg%IPERM=1
C     ETAP(JP1)=mean water level above datum inside permeable layer
C     where ETA(JP1) and ETAP(JP1) are mean water levels above datum
            IF(cg%IPERM.EQ.1) THEN
              IF(cg%HP(JP1,L).LE.0.D0) THEN
                cg%UPMWD(JP1)=0.D0
                cg%QP(JP1)=0.D0
                WPGH=0.D0
              ELSE
                ETA(JP1)=cg%HWD(JP1)+cg%ZB(JP1,L)
                DUM=cg%ZP(JP1,L)
                IF(DUM.LT.cg%SWLBC(ITIME).AND.cg%ZP(JP1,L).GE.
     +             cg%ZP(J,L))
     +             DUM=cg%SWLBC(ITIME)
                ETAP(JP1)=cg%ZB(JP1,L)*cg%PWET(JP1)+DUM*
     +                    (1.D0-cg%PWET(JP1))
                IF(ETAP(JP1).LT.cg%ZP(JP1,L)) ETAP(JP1)=cg%ZP(JP1,L)
                C=(ETA(JP1)-ETA(J))/cg%DX
                DUM=DSQRT(cg%ALSTA2+cg%BE4*DABS(C))
                cg%UPMWD(JP1)=(DUM-cg%ALSTA)/cg%BE2
                IF(C.GT.0.D0) cg%UPMWD(JP1)=-cg%UPMWD(JP1)
                cg%QP(JP1)=cg%UPMWD(JP1)*(ETAP(JP1)-cg%ZP(JP1,L))*
     +                     cg%PWET(JP1)
                DUM=cg%QO(L)-QWAVE
               IF(cg%QP(JP1).LT.DUM.AND.DABS(cg%QP(JP1)).GT.1.D-5) THEN
                  cg%UPMWD(JP1)=cg%UPMWD(JP1)*DUM/cg%QP(JP1)
                  cg%QP(JP1)=DUM
                ENDIF
                DUM=cg%WPM*cg%DX*0.5D0*(cg%PWET(JP1)+cg%PWET(J))
                WPGH=PMGH*DUM/DSQRT(0.5D0*(cg%HWD(JP1)+cg%HWD(J)))
              ENDIF
              cg%QWX=cg%QO(L)-cg%QP(JP1)
              A=cg%QWX*cg%QWX/BGH3
            ENDIF
            IF(cg%INFILT.EQ.1) cg%QP(JP1)=0.D0
C
            QS = cg%QWX-QWAVE
C     QS=return flux must be zero or negative for J<cg%JCREST
            IF(QS.GT.0.D0) QS=0.D0
            cg%USWD(JP1) = QS/cg%HWD(JP1)
            DUM=cg%QWX-QS
            IF(DABS(DUM).LT.1.D-3) THEN
              R=0.D0
            ELSE
              R = cg%CWD*QS/DUM
            ENDIF
            IF(cg%IVEG.EQ.0)THEN
              DG(JP1) = AH*cg%DX*cg%FB2(JP1,L)*GBWD(R)
C BDJ 2011->2014 on 2014-10-02
C  added to kill friction in wetdry
              DG(JP1) = 0.D0
C end BDJ 2011->2014 on 2014-10-02
            ELSE
              DUM=cg%HWD(JP1)/cg%PWET(JP1)
              X=cg%VEGH(JP1,L)/DUM
              DG(JP1)=AH*cg%DX*(cg%FB2(JP1,L)*GBWD(R)+cg%VEGINP(JP1,L)*
     +         DUM*GDWD(R
     +        ,X))
            ENDIF
            DUM=0.5D0*(DG(J)+DG(JP1))+WPGH
            IF(DABS(DUM-DGJP1).GT.1.D-5) THEN
              DGJP1 = DUM
              GOTO 200
            ELSE
              G(JP1)=G(J)+DUM
C     LSTART=2 indicates that bottom elevation is peaked at node JC
              IF(JP1.LT.cg%JMAX(L)) THEN
                IF(cg%ZB(J+2,L).LT.cg%ZB(JP1,L)) THEN
                  JC=JP1
                  HC=cg%HWD(JC)
                  PC = cg%PWET(JC)
                  QWC=cg%QWX
                  LSTART=2
                ELSE
                  IF(J.EQ.cg%JWD) LSTART=1
                ENDIF
              ENDIF
              GOTO 220
            ENDIF
 200      CONTINUE
        ELSE
C
C---------------------- BOTTOM ELEVATION DECREASING LANDWARD OR J>cg%JCREST --------------
C     On the landward slope (J>cg%JCREST) or downslope zone for J<cg%JCREST or ponded water
C     zone, cg%PWET=constant on impermeable bottom and compute cg%HWD and cg%USWD for IUPSLP=0
          IF(LSTART.EQ.2) THEN
            PCI=1.D0/PC
            QWC2=QWC*QWC
            BG=cg%BWD*cg%GRAV
            CPC = 0.5D0*PC/cg%BWD/HC
            AB = 0.25D0*PC*QWC2/(BG*HC*HC*HC)
            G(J) = 0.D0
            QS=cg%USWD(J)*cg%HWD(J)
            DUM=QWC-QS
            IF(DABS(DUM).LT.1.D-3) THEN
              R=0.D0
            ELSE
              R=cg%CWD*QS/DUM
            ENDIF
            IF(cg%IVEG.EQ.0)THEN
              DG(J)=cg%AGWD*cg%DX*cg%FB2(J,L)*GBWD(R)
            ELSE
              DUM=cg%HWD(J)/cg%PWET(J)
              X=cg%VEGH(J,L)/DUM
              DG(J)=cg%AGWD*cg%DX*(cg%FB2(J,L)*GBWD(R)+cg%VEGINP(J,L)*
     +              DUM*GDWD(R,X))
            ENDIF
            LSTART=0
          ENDIF
          DZB=cg%ZB(JC,L)-cg%ZB(JP1,L)
          IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
            IF(JP1.GE.cg%JXW.AND.JP1.LE.cg%JX2) THEN
              IF(cg%JX2.LT.cg%JMAX(L)) DZB=cg%ZB(JC,L)-cg%ZW
              cg%QWX=cg%QO(L)-(cg%QO(L)-cg%QM)*(cg%XB(JP1)-
     +               cg%XB(cg%JXW))/(cg%XB(cg%JX2)-cg%XB(cg%JXW))
              A=cg%QWX*cg%QWX/BGH3
            ENDIF
          ENDIF
          IF(cg%IPERM.EQ.0) THEN
            WPGH=0.D0
            IF(cg%INFILT.EQ.1) THEN
              WPGH=PMG1*cg%WPM*cg%PWET(J)*cg%DX/DSQRT(cg%HWD(J))
            ENDIF
          ELSE
            IF(cg%HP(JP1,L).LE.0.D0) THEN
              WPGH=0.D0
            ELSE
              DUM=0.5D0*(cg%HP(J,L)+cg%HP(JP1,L))/cg%SDP
C             IF(DUM.GT.10.D0) DUM=10.D0
              PMG=PMG1*DUM**0.3D0
              WPGH=PMG*cg%WPM*cg%PWET(J)*cg%DX/DSQRT(cg%HWD(J))
            ENDIF
          ENDIF
          DGJP1 = DG(J)+WPGH
          DO 210 ITEH=1,20
            G(JP1)= G(J)+DGJP1
            C=CPC*(DZB-G(JP1))
            IF(C.LT.0.D0) C=0.D0
            IF(HC.GT.1.D-6) THEN
              Y= cg%HWD(J)/HC
            ELSE
              cg%JDRY=J
              GOTO 110
            ENDIF
 205        DUM=1.D0/Y/Y
            F=Y-1.D0+AB*(DUM-1.D0)-C
            DF=1.D0-2.D0*AB*DUM/Y
            IF(DABS(DF).LT.1.D-6) THEN
              cg%JDRY=J
              GOTO 110
            ENDIF
            YNEW=Y-F/DF
            IF(DABS(YNEW-Y).GT.1.D-6) THEN
              Y=YNEW
              GOTO 205
            ENDIF
            cg%HWD(JP1)=YNEW*HC
            IF(cg%HWD(JP1).LT.1.D-6) THEN
              cg%JDRY=J
              GOTO 110
            ENDIF
            IF(cg%HWD(JP1).GT.cg%HWD(J)) cg%HWD(JP1)=cg%HWD(J)
            IF(cg%IPERM.EQ.0.AND.cg%INFILT.EQ.0) THEN
              cg%PWET(JP1)=PC
            ELSE
              DUM=PCI+(QWC2-cg%QWX*cg%QWX)/BG/cg%HWD(JP1)**3.D0
C             IF(DUM.LE.0.D0) THEN
C               cg%JDRY=J
C               GOTO 110
C             ENDIF
              IF(DUM.LT.PCI) DUM=PCI
              cg%PWET(JP1)=1.D0/DUM
              IF(cg%PWET(JP1).GT.cg%PWET(J)) cg%PWET(JP1)=cg%PWET(J)
            ENDIF
            QWAVE=cg%AQWD*cg%HWD(JP1)*DSQRT(cg%GRAV*cg%HWD(JP1)/
     +            cg%PWET(JP1))
C
C     Compute cg%QP and cg%UPMWD in permeable layer if cg%IPERM=1 as above
            IF(cg%IPERM.EQ.1) THEN
              IF(cg%HP(JP1,L).LE.0.D0) THEN
                cg%UPMWD(JP1)=0.D0
                cg%QP(JP1)=0.D0
                WPGH=0.D0
              ELSE
                ETA(JP1)=cg%HWD(JP1)+cg%ZB(JP1,L)
                DUM=cg%ZP(JP1,L)
                IF(DUM.LT.cg%SWLBC(ITIME).AND.cg%ZP(JP1,L).GE.
     +             cg%ZP(J,L))
     +             DUM=cg%SWLBC(ITIME)
                ETAP(JP1)=cg%ZB(JP1,L)*cg%PWET(JP1)+DUM*
     +                    (1.D0-cg%PWET(JP1))
                IF(ETAP(JP1).LT.cg%ZP(JP1,L)) ETAP(JP1)=cg%ZP(JP1,L)
                C=(ETA(JP1)-ETA(J))/cg%DX
                DUM=DSQRT(cg%ALSTA2+cg%BE4*DABS(C))
                cg%UPMWD(JP1)=(DUM-cg%ALSTA)/cg%BE2
                IF(C.GT.0.D0) cg%UPMWD(JP1)=-cg%UPMWD(JP1)
                cg%QP(JP1)=cg%UPMWD(JP1)*(ETAP(JP1)-cg%ZP(JP1,L))*
     +                    cg%PWET(JP1)
C               DUM=cg%QO(L)-QWAVE
C               IF(J.GE.cg%JCREST(L).AND.cg%QP(JP1).GT.DUM) THEN
C                 cg%UPMWD(JP1)=cg%UPMWD(JP1)*DUM/cg%QP(JP1)
C                 cg%QP(JP1)=DUM
C               ENDIF
                DUM=cg%WPM*cg%DX*0.5D0*(cg%PWET(JP1)+cg%PWET(J))
                WPGH=PMG*DUM/DSQRT(0.5D0*(cg%HWD(JP1)+cg%HWD(J)))
              ENDIF
              cg%QWX=cg%QO(L)-cg%QP(JP1)
              A=cg%QWX*cg%QWX/BGH3
            ENDIF
C
C     Compute cg%QP(J)=infiltration rate landward of dune crest if cg%INFILT=1
            IF(cg%INFILT.EQ.1) THEN
              IF(JP1.GT.cg%JCREST(L)) THEN
                cg%QP(JP1)=cg%QP(J)+0.5D0*cg%DX*cg%WPM*(cg%PWET(J)+
     +                    cg%PWET(JP1))
                DUM=cg%WPM*cg%DX*0.5D0*(cg%PWET(JP1)+cg%PWET(J))
                WPGH=PMG1*DUM/DSQRT(0.5D0*(cg%HWD(JP1)+cg%HWD(J)))
                cg%QWX=cg%QO(L)-cg%QP(JP1)
                A=cg%QWX*cg%QWX/BGH3
              ELSE
                cg%QP(JP1)=0.D0
                WPGH=0.D0
              ENDIF
            ENDIF
            QS = cg%QWX - QWAVE
C           QS= steady flux on landward slope (J>cg%JCREST) must be zero or positive
            IF(cg%IPOND.EQ.0) THEN
              IF(J.GE.cg%JCREST(L).AND.QS.LT.0.D0) QS=0.D0
            ENDIF
            IF(cg%HWD(JP1).LT.1.D-3.AND.QS.GT.1.D-3) QS=1.D-3
            cg%USWD(JP1) = QS/cg%HWD(JP1)
            DUM=cg%QWX-QS
            IF(DABS(DUM).LT.1.D-3) THEN
              R=0.D0
            ELSE
              R=cg%CWD*QS/DUM
            ENDIF
            IF(cg%IVEG.EQ.0)THEN
              DG(JP1)=cg%AGWD*cg%DX*cg%FB2(JP1,L)*GBWD(R)
            ELSE
              DUM=cg%HWD(JP1)/cg%PWET(JP1)
              X=cg%VEGH(JP1,L)/DUM
              DG(JP1)=cg%AGWD*cg%DX*(cg%FB2(JP1,L)*GBWD(R)+
     +                    cg%VEGINP(JP1,L)*DUM*GDWD
     +        (R,X))
            ENDIF
            DUM=0.5D0*(DG(J)+DG(JP1))+WPGH
            IF(DABS(DUM-DGJP1).GT.1.D-5) THEN
              DGJP1 = DUM
              GOTO 210
            ELSE
              G(JP1)=G(J)+DUM
C     LSTART=1 indicates beginning of upslope computation
            IF(cg%IPOND.EQ.0.OR.cg%NOPOND.EQ.1) THEN
              IF(JP1.LT.cg%JCREST(L)) THEN
                IF(cg%ZB(J+2,L).GE.cg%ZB(JP1,L)) LSTART=1
              ENDIF
              ELSE
                IF(JP1.EQ.cg%JX2) THEN
                  LSTART=1
                  cg%QWX=cg%QM
                  A=cg%QWX*cg%QWX/BGH3
                ENDIF
              ENDIF
              GOTO 220
            ENDIF
 210      CONTINUE
        ENDIF
C----------------- END OF BOTTOM ELEVATION INCREASING OR DECREASING ----------------
C
C     Compute mean velocity cg%UMEAWD and standard deviations
C     cg%SIGWD and cg%USTDWD in wet and dry zone
 220    cg%UMEAWD(JP1)=cg%AUWD*DSQRT(cg%GRAV*cg%PWET(JP1)*cg%HWD(JP1))
     +     +cg%PWET(JP1)*cg%USWD(JP1)
C BDJ 2011->2014 on 2014-10-02
C        cg%SIGWD(JP1)=cg%HWD(JP1)*DSQRT(2.D0/cg%PWET(JP1)-2.D0+cg%PWET(JP1))
        cg%SIGWD(JP1)=CORRECT*cg%HWD(JP1)*DSQRT(2.D0/cg%PWET(JP1)-
     +                    2.D0+cg%PWET(JP1))
C end BDJ 2011->2014 on 2014-10-02
        DUM = cg%UMEAWD(JP1) - cg%USWD(JP1)
        DUM1 = cg%PWET(JP1)*DUM**2.D0-2.D0*DUM*(cg%UMEAWD(JP1)-
     +     cg%PWET(JP1)*cg%USWD(JP1))
        DUM = cg%AGWD*cg%GRAV*cg%HWD(JP1)+DUM1
        IF(DUM.GT.0.D0) THEN
          cg%USTDWD(JP1) = DSQRT(DUM)
        ELSE
          cg%JDRY=J
          GOTO 110
        ENDIF
        IF(cg%IANGLE.EQ.1) THEN
          cg%STHETA(JP1)=cg%STHETA(cg%JWD)
          cg%VMEAWD(JP1)=cg%AUWD*DSQRT(cg%GRAV*cg%PWET(JP1)*
     +                    cg%HWD(JP1))*cg%STHETA(JP1)
          DUM=1.D0-0.25D0*cg%PI*cg%PWET(JP1)*(2.D0-cg%PWET(JP1))
          cg%VSTDWD(JP1)=cg%AWD*DSQRT(cg%GRAV*cg%HWD(JP1)*DUM)*
     +                    DABS(cg%STHETA(JP1))
        ENDIF
C     Mean water depth limited by cg%HWDMIN specified in Subr.2 INPUT
C     Horizontal distance of wet and dry zone is limited because of
C     assumed alongshore uniformity
C       DUM=(cg%XB(JP1)-cg%XB(cg%JWD))/cg%H1
C       IF(cg%HWD(JP1).LT.cg%HWDMIN.OR.DUM.GT.1000D0) THEN
        IF(cg%HWD(JP1).LT.cg%HWDMIN) THEN
          cg%JDRY = JP1
C       IF(DUM.GT.1000D0.AND.JP1.GT.cg%JCREST(L)) cg%JMAX(L)=JP1
          GOTO 110
        ENDIF
C
        IF(J.EQ.JEND) cg%JDRY=JP1
 100  CONTINUE
C-------------------END OF LANDWARD MARCHING --------------------------
C
 110  CONTINUE
C
C     cg%QOTF=Combined overtopping and overflow rate cg%QOTF
C     cg%SPRATE=seepage rate through permeable layer predicted by modified
C     formula of Kobayashi and de los Santos(2007) for no overtopping
C     where cg%USWD=0.0(unidirectional flow) at cg%JCREST is assumed
      cg%QOTF=0.D0
      cg%SPRATE=0.D0
      JDAM=cg%JSWL(L)
      IF(cg%IPROFL.EQ.2) JDAM=cg%JCREST(L)
      IF(cg%JDRY.GE.cg%JCREST(L).AND.JDAM.LT.cg%JMAX(L)) THEN
        J=cg%JCREST(L)
        IF(cg%JWD.EQ.cg%JMAX(L)) J=cg%JMAX(L)
        cg%QOTF = cg%AQWD*cg%HWD(J)*DSQRT(cg%GRAV*cg%HWD(J)/cg%PWET(J))
        IF(cg%IPERM.EQ.1) THEN
          IF(cg%JDRY.EQ.cg%JMAX(L).OR.cg%IWTRAN.EQ.1) THEN
            cg%SPRATE=cg%QP(cg%JCREST(L))
            IF(cg%SPRATE.LT.0.D0) cg%SPRATE=0.D0
C         ELSE
C           IF(cg%IWTRAN.EQ.0) cg%QOTF=0.D0
          ENDIF
        ENDIF
      ENDIF
      IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
        cg%QD=cg%QOTF
        IF(cg%JDRY.EQ.cg%JMAX(L)) THEN
          IF(cg%ZW.LT.cg%ZB(cg%JMAX(L),L)) THEN
            cg%QM=cg%AQWD*cg%HWD(cg%JMAX(L))*DSQRT(cg%GRAV
     +                    *cg%HWD(cg%JMAX(L))/cg%PWET(cg%JMAX(L)))
            IF(cg%QM.GT.cg%QOTF) cg%QM=cg%QOTF
          ELSE
            cg%QM=cg%QOTF
          ENDIF
        ELSE
          cg%QM=0.D0
        ENDIF
        IF(cg%JCREST(L).EQ.cg%JXW) cg%QOTF=cg%QM
      ENDIF
      IF(cg%IPERM.EQ.1.AND.cg%QOTF.EQ.0.D0) THEN
C     Find node JDUM for highest and most landward point of cg%ZP(J,L)
        JSEP=cg%JR
        IF(cg%ZB(JSEP,L).LE.cg%SWLBC(ITIME)) GOTO 301
        IF(cg%IWTRAN.EQ.0) THEN
          JDUM=JSEP
          DUM=cg%ZP(JSEP,L)
          DO 300 J=(JSEP+1),cg%JMAX(L)
            IF(cg%ZP(J,L).GE.DUM) THEN
              DUM=cg%ZP(J,L)
              JDUM=J
            ENDIF
 300      CONTINUE
          DETA=cg%ZB(JSEP,L)-cg%ZP(JDUM,L)
        ELSE
          JDUM=cg%JSL1
          DETA=cg%ZB(JSEP,L)-cg%ZB(JDUM,L)
        ENDIF
        IF(DETA.GT.0.D0) THEN
          DUM=cg%XB(JDUM)-cg%XB(JSEP)
          IF(DUM.LE.0.D0) GOTO 301
          cg%SPRATE=0.2D0*DETA**1.5D0/DSQRT(cg%BESTA1*DUM)
        ENDIF
 301    CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE WETDRY
C
C     -16---------------- END OF SUBROUTINE WETDRY -------------------------

C     **********************************************************************
C     This function related to bottom shear stress in wet and dry zone
C     is called from Subr.16 WETDRY
      FUNCTION GBWD(R)
      DOUBLE PRECISION R,R2,GBWD
      IF(R.GE.0.D0) THEN
        GBWD = 1.D0 + 1.77245D0*R + R*R
      ELSE
        R2 = R*R
        GBWD = 2.D0*DEXP(-R2)-R2-1.D0+1.77245D0*R*(3.D0-2.D0*ERFCC(R))
      ENDIF
C     Complementary error function ERFCC below Subr.06 GBXAGF
      RETURN
      END FUNCTION GBWD
C     ***********************************************************************


C     ***********************************************************************
C     This function related to vegetation drag force in wet and and dry zone
C     is called from Subr.16 WETDRY
      FUNCTION GDWD(R,X)
      DOUBLE PRECISION GDWD,R,X,EX,SX,FX,R2,ER2,FR,C
C     IF(X.LE.0.D0) THEN
C       GDWD = 0.D0
C     ELSE
C       GDWD=2.D0-(X+2.D0)*DEXP(-X)
        EX=DEXP(-X)
        SX=DSQRT(X)
        FX=1.D0-ERFCC(SX)
        R2 = R*R
        IF(R.GE.0.D0)THEN
          GDWD=2.D0-(X+2.D0)*EX+R*(1.77245D0*X-3.D0*SX*EX+1.77245D0*(1.5
     +    D0-X)*FX)+R2*(1.D0-EX)
        ELSE
          ER2=DEXP(-R2)
          FR=1.D0-ERFCC(R)
          C=(X+2.D0+R2+3.D0*R*SX)*EX
          IF(X.LE.R2)THEN
           GDWD=2.D0*X*ER2-2.D0-R2+1.77245D0*R*((X-1.5D0)*FX+2.D0*X*FR+X
     +     )+C
          ELSE
           GDWD=4.D0*ER2-2.D0-R2+5.317362D0*R*FR+1.77245D0*R*(X+(1.5D0-X
     +     )*FX)-C
          ENDIF
        ENDIF
C     ENDIF
      RETURN
      END FUNCTION GDWD
C     ***********************************************************************


C     ********************************************************************
      FUNCTION ERFCC(X)
      DOUBLE PRECISION X, Z, T, ERFCC
      Z=DABS(X)
      T=1.D0/(1.D0+0.5D0*Z)
      ERFCC=T*DEXP(-Z*Z-1.26551223D0+T*(1.00002368D0+T*(.37409196D0+
     +   T*(.09678418D0+T*(-.18628806D0+T*(.27886807D0+
     +   T*(-1.13520398D0+T*(1.48851587D0+
     +   T*(-.82215223D0+T*.17087277D0)))))))))
      IF (X.LT.0.D0) ERFCC=2.D0-ERFCC
      RETURN
      END FUNCTION ERFCC
C     *********************************************************************


C     #12##################### SUBROUTINE CHANGE ###########################
C
C     Compute the bottom elevation change using the volume conservation
C     of bottom sediment.
C
      SUBROUTINE CHANGE(cg,ITIME,L,IEND,ICALL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000, NB=30000,NL=100)
      DIMENSION DZBDT(NN),CB(NN),R(NN),DELZBRW(NN),DELZBJ(NN),VDUM(NN),
     +   V(NL),VDY(NL),AVY(NL),ADZX(NL)
C
C
C     If ICALL=1, alonshore uniformity is assumed for profile change
C     Compute the first-order rate of the bottom elevation change where
C     sediment transport rate cg%Q(J) computed in Subr.11 SEDTRA.
C     The seaward boundary location at node 1 is chosen such that
C     bottom change is negligible seaward of node 1. Also at node cg%JMAX
      IF(ICALL.EQ.1) THEN
      JMAXM1 = cg%JMAX(L) - 1
      DZBDT(1) = 0.D0
      DZBDT(cg%JMAX(L)) = 0.D0
      DO 100 J = 2,JMAXM1
        DZBDT(J) = (cg%Q(J-1)-cg%Q(J+1))/cg%DX2
 100  CONTINUE
      IF(cg%IVWALL(L).EQ.2) DZBDT(JMAXM1) = (cg%Q(cg%JMAX(L)-2) -
     +   cg%Q(JMAXM1))/cg%DX
C     where backward finite difference is used at the node next to the wall
C     if the vertical wall is exposed to wave action.
C
C     Find the cg%TIME step cg%DELT using the numerical stability criterion
C     but the value of cg%DELT is limited by the end cg%TIME cg%TIMEBC(ITIME+1)
C     for given cg%TIME
C     Compute CB(J)=bottom profile phase velocity
C     CBMAX = 0.001D0
C     Increase of CBMAX tends to reduce cg%DELT and improve numerical stability
C     CBMAX=0.05D0
      CBMAX=0.004D0
      DZBMAX=0.1D0*cg%DX
      DO 115 J=1,cg%JMAX(L)
        IF(J.EQ.1) THEN
          DELQ = cg%Q(2) - cg%Q(1)
          DELZB1 = cg%ZB(2,L) - cg%ZB(1,L)
        ELSEIF(J.EQ.cg%JMAX(L)) THEN
          J1 = JMAXM1
          DELQ = cg%Q(J) - cg%Q(J1)
          DELZB1 = cg%ZB(J,L) - cg%ZB(J1,L)
        ELSE
          JP1 = J+1
          JM1 = J-1
          DELQ = cg%Q(JP1) - cg%Q(JM1)
          DELZB1 = cg%ZB(JP1,L) - cg%ZB(JM1,L)
        ENDIF
        IF(DABS(DELZB1).GT.DZBMAX) THEN
          CB(J) = DELQ/DELZB1
        ELSE
          CB(J) =0.D0
        ENDIF
        DUMC = DABS(CB(J))
        IF(DUMC.GT.CBMAX) CBMAX=DUMC
 115  CONTINUE
      cg%DELT = cg%DX/CBMAX
      IDUM = ITIME + 1
      DUM = (cg%TIMEBC(IDUM) - cg%TIMEBC(ITIME))/2.D0
      IF(cg%DELT.GT.DUM) cg%DELT=DUM
C
      DUM = cg%TIME+cg%DELT
      IF(DUM.GE.cg%TIMEBC(IDUM)) THEN
        cg%DELT = cg%TIMEBC(IDUM) - cg%TIME
        IEND = 1
      ENDIF
C
C     Compute DELZBRW(J)=first-order bottom elevation change
C     before smoothing
      DO 120 J = 1,cg%JMAX(L)
        DELZBRW(J) = cg%DELT*DZBDT(J)
 120  CONTINUE
C
C     Add second-order correction to DELZBRW(J)
      DTDT = cg%DELT*cg%DELT
      DO 121 J=1,cg%JMAX(L)
        R(J) = DTDT*CB(J)*CB(J)/cg%DXDX
 121  CONTINUE
      DO 122 J=2,JMAXM1
        JP1 = J+1
        JM1 = J-1
        DUM = cg%ZB(JP1,L)*(R(JP1)+R(J))/4.D0-cg%ZB(J,L)*(R(J)/2.D0+
     +     (R(JP1)+R(JM1))/4.D0)+cg%ZB(JM1,L)*(R(J)+R(JM1))/4.D0
        DELZBRW(J) = DELZBRW(J)+ DUM
 122  CONTINUE
C
C     If cg%ISTSAN=1, erosion is limited by available sand in stone structure
      IF(cg%ISTSAN.EQ.1) THEN
          DO 123 J=1,cg%JMAX(L)
              IF(cg%HPSTON(J,L).GT.0.D0) THEN
                  DUM=cg%ZP(J,L)-cg%ZBSTON(J,L)
                  IF(DUM.GT.0.D0) THEN
                      cg%VDSAND(J)=DUM+cg%SNP*cg%HPSTON(J,L)
                  ELSE
                      cg%VDSAND(J)=cg%SNP*(cg%ZP(J,L)-cg%ZPSTON(J,L))
                  ENDIF
                  DUM=cg%VDSAND(J)+DELZBRW(J)
                  IF(DUM.LT.0.D0) DELZBRW(J)=-cg%VDSAND(J)
               ELSE
                  cg%VDSAND(J)=0.D0
               ENDIF
 123  CONTINUE
      ENDIF
C
C     Smoothing DELZBRW using Subr.15 SMOOTH
      JMAXL=cg%JMAX(L)
      CALL SMOOTH(cg, JMAXL,DELZBRW,DELZBJ)
      IF(cg%ISEDAV.EQ.2) THEN
          DO 125 J=1, JMAXL
              DUM=DELZBJ(J)+cg%ZB(J,L)
              IF(DUM.LT.cg%ZMESH(J,L)) DELZBJ(J)=cg%ZMESH(J,L)-
     +              cg%ZB(J,L)
 125      CONTINUE
      ENDIF
C
C     Adjust smoothed bottom elevation change cg%DELZB
C     to satisfy the volume conservation between J=1 to cg%JMAX
      DUM = cg%DELT*(cg%Q(1)-cg%Q(cg%JMAX(L)))
      CALL INTGRL(JMAXL,cg%DX,DELZBJ,AREA)
      ADJUST = (DUM-AREA)/(cg%XB(cg%JMAX(L))-cg%XB(1))
      DO 130 J=1,cg%JMAX(L)
        cg%DELZB(J,L) = ADJUST+DELZBJ(J)
 130  CONTINUE
C     If cg%ISTSAN=1, cg%ZB(J,L) and cg%ZP(J,L) at next cg%TIME level are computed as follows
      IF(cg%ISTSAN.EQ.1) THEN
      DO 140 J=1,cg%JMAX(L)
              IF(cg%HPSTON(J,L).LE.0.D0) THEN
                  cg%ZB(J,L)=cg%ZB(J,L)+cg%DELZB(J,L)
                  cg%ZP(J,L)=cg%ZB(J,L)
              ELSE
                  IF(cg%DELZB(J,L).GE.0.D0) THEN
                      DUM=cg%SNP*cg%HP(J,L)
                      IF(cg%DELZB(J,L).GE.DUM) THEN
                          cg%ZB(J,L)=cg%ZB(J,L)+cg%DELZB(J,L)-DUM
                          cg%ZP(J,L)=cg%ZB(J,L)
                      ELSE
                          cg%ZB(J,L)=cg%ZBSTON(J,L)
                          cg%ZP(J,L)=cg%ZP(J,L)+cg%DELZB(J,L)/cg%SNP
                      ENDIF
                  ELSE
                      IF(cg%HP(J,L).GT.0.D0) THEN
                          cg%ZB(J,L)=cg%ZBSTON(J,L)
                          cg%ZP(J,L)=cg%ZP(J,L)+cg%DELZB(J,L)/cg%SNP
                      ELSE
                          DUM=cg%ZB(J,L)-cg%ZBSTON(J,L)+cg%DELZB(J,L)
                          IF(DUM.GE.0.D0) THEN
                              cg%ZB(J,L)=cg%ZB(J,L)+cg%DELZB(J,L)
                              cg%ZP(J,L)=cg%ZB(J,L)
                          ELSE
                              cg%ZB(J,L)=cg%ZBSTON(J,L)
                              cg%ZP(J,L)=cg%ZBSTON(J,L)+DUM/cg%SNP
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
 140  CONTINUE
      ENDIF
C
      ENDIF
C..........End of ICALL=1..................................................
C
C     If ICALL=2 from Main Program, the profile change due to alongshore
C     gradient of longshore sediment transport is included if cg%IQYDY=1 and
C     computed when IEND=1 and L=cg%ILINE
      IF(ICALL.EQ.2) THEN
      DO 200 LL=1, cg%ILINE
        JMAXL=cg%JMAX(LL)
      DO 210 J=1,JMAXL
        R(J)=cg%VY(J,LL)
        DELZBRW(J)=DABS(cg%ZB(J,LL)-cg%DZX(J,LL))
 210  CONTINUE
      CALL SMOOTH(cg,JMAXL,R,CB)
      CALL SMOOTH(cg,JMAXL,DELZBRW,DELZBJ)
      CALL INTGRL(JMAXL,cg%DX,CB,AREA)
      AVY(LL)=AREA
      CALL INTGRL(JMAXL,cg%DX,DELZBJ,AREA)
      ADZX(LL)=AREA
      DO 211 J=1,JMAXL
        cg%VY(J,LL)=CB(J)
        cg%DZX(J,LL)=DELZBJ(J)
 211  CONTINUE
 200  CONTINUE
c     bdj added on 2019-02-07
      CALL SMOOTH(cg,cg%ILINE,RESHAPE(AVY,SHAPE(DZBDT),pad=[0.D0]),VDUM)
      V = VDUM(1:size(AVY))
c     CALL SMOOTH(cg%ILINE,AVY,V)
c     end bdj added on 2019-02-07
      ILINE1=cg%ILINE-1
      DO 220 LL=1, ILINE1
        VDY(LL)=(V(LL+1)-V(LL))/cg%DYLINE(LL)
 220  CONTINUE
      AVY(1)=VDY(1)
      AVY(cg%ILINE)=VDY(ILINE1)
      DO 230 LL=2, ILINE1
C     Use upstream finite difference method
      DUM=V(LL)*cg%DYLINE(LL)
      IF(DUM.GE.0.D0) THEN
        AVY(LL)=VDY(LL-1)
      ELSE
        AVY(LL)=VDY(LL)
      ENDIF
 230  CONTINUE
      DO 240 LL=1, cg%ILINE
        IF(ADZX(LL).LT.1.D-6) THEN
          AVY(LL)=AVY(LL)/1.D-6/cg%SPORO1
        ELSE
          AVY(LL)=AVY(LL)/ADZX(LL)/cg%SPORO1
        ENDIF
 240  CONTINUE
      DO 250 LL=1, cg%ILINE
        JMAXL=cg%JMAX(LL)
C       DUM=cg%XB(JMAXL)*cg%SPORO1
        DO 260 J=1, JMAXL
          DELZBRW(J)=-cg%DZX(J,LL)*AVY(LL)
C         DELZBRW(J)=-AVY(LL)/DUM
 260  CONTINUE
      CALL SMOOTH(cg,JMAXL,DELZBRW,DELZBJ)
      DO 270 J=1, JMAXL
        cg%ZB(J,LL)=DELZBJ(J)+cg%ZB(J,LL)
        IF(cg%IPERM.EQ.1.OR.cg%ISEDAV.GE.1) THEN
          cg%HP(J,LL)=cg%ZB(J,LL)-cg%ZP(J,LL)
          IF(cg%HP(J,LL).LT.0.D0) THEN
            cg%HP(J,LL)=0.D0
            cg%ZB(J,LL)=cg%ZP(J,LL)
          ENDIF
          IF(cg%ISEDAV.EQ.2) THEN
            IF(cg%ZB(J,LL).LT.cg%ZMESH(J,LL)) cg%ZB(J,LL)=
     +            cg%ZMESH(J,LL)
          ENDIF
        ENDIF
 270  CONTINUE
 250  CONTINUE
      ENDIF
C
C..........End of ICALL=2..................................................
C
      RETURN
      END SUBROUTINE CHANGE
C
C     -12-----------------  END OF SUBROUTINE CHANGE  ---------------------


C     #07#####################  SUBROUTINE DBREAK  ########################
C
C     This subroutine calculates QBREAK and DBSTA for wave breaking
C
      SUBROUTINE DBREAK(cg,J, L, WHRMS, D)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NL=100)
C
c mg
C     COMMON /PREDIC/ HRMS(NN),SIGMA(NN),H(NN),WSETUP(NN),SIGSTA(NN)
C mg
C     Calculate energy dissipation factor cg%ABREAK(J) for steep slope
C     where D = mean water depth from Main Program
      cg%ABREAK(J) = (cg%TWOPI/cg%WKP/D)*cg%BSLOPE(J,L)*cg%CTHETA(J)
     +          /3.D0
      IF(cg%ABREAK(J).LT.1.D0) cg%ABREAK(J) = 1.D0
C mg
C mg  Allow for variable cg%GAMMA
C     IF(cg%GAMMA.LT.0) THEN
C mg  Compute deep water wave height
C       CO = cg%GRAV*cg%TP/cg%TWOPI
C         THETAO=DASIN(CO/cg%CP(1)*cg%STHETA(1))
C       HRMSO = HRMS(1)*DSQRT((cg%CP(1)*cg%WN(1)*cg%CTHETA(1))/
C    +    (0.5D0*CO*DCOS(THETAO)))
C mg  Alex Apotsos et al. 2008, Coastal Engineering 55 (2008) 224-235.  (Eq 23)
C       GAMMA_TEMP = 0.3 + 0.45*TANH(0.9*HRMSO)
C     ELSE
C       GAMMA_TEMP = cg%GAMMA
C     ENDIF
C mg
C     ... FRACTION OF BREAKING WAVES AND ASSOCIATED DISSIPATION
C
C     cg%QBREAK(J) = Fraction of breaking waves at node J
C     cg%DBSTA(J)  = Time averaged normalized energy dissipation due to
C     wave breaking at node J
C mg
      HM = 0.88D0/cg%WKP*DTANH(cg%GAMMA*cg%WKP*D/0.88D0)
C mg  HM = 0.88D0/cg%WKP*DTANH(GAMMA_TEMP*cg%WKP*D/0.88D0)
C mg
C     Compute cg%QBREAK = fraction of breaking waves
      B = (WHRMS/HM)**2.D0
C     IF(B.LT.0.99999D0) THEN
      IF(B.LT.0.99999D0.AND.WHRMS.GT.1.D-10) THEN !bdj
        QBOLD = B/2.D0
 10     cg%QBREAK(J) = QBOLD - (1.D0-QBOLD + B*DLOG(QBOLD))/
     +          (B/QBOLD-1.D0)
        IF(cg%QBREAK(J).LE.0.D0) cg%QBREAK(J) = QBOLD/2.D0
        IF(DABS(cg%QBREAK(J)-QBOLD).GT.1.D-6) THEN
          QBOLD = cg%QBREAK(J)
          GOTO 10
        ENDIF
      ELSE
        cg%QBREAK(J) = 1.D0
        IF(WHRMS.LE.1.D-10) cg%QBREAK(J)=0.D0
        HM=WHRMS
      ENDIF
C
      cg%DBSTA(J) = 0.25D0*cg%ABREAK(J)*cg%QBREAK(J)*HM*HM/cg%WT(J)
C
C     Reduce SIGSTA if WHRMS is larger than cg%GAMMA*D
C     (used only for wave transmission over submerged breakwater)
C     GAMD = cg%GAMMA*D
C     IF(WHRMS.LE.GAMD) THEN
      cg%SISMAX = 1.D0
C     ELSE
C     cg%SISMAX = DSQRT(cg%GAMMA*WHRMS/D/8.0D0)
C     ENDIF
C
      RETURN
      END SUBROUTINE DBREAK
C
C     -07-----------------  END OF SUBROUTINE DBREAK  ---------------------


c     #27##################### SUBROUTINE DISPERSION ##############################
      SUBROUTINE DISPERSION(DEPTH, PERIOD, WKZ)
C
      DOUBLE PRECISION DEPTH, PERIOD, WAVENUMTMP, SEGMA, FK, WKZ
      DOUBLE PRECISION PI, GRAV
      PI = 3.1415926D0
      GRAV=9.81D0
      SEGMA  = 2.0D0*PI / PERIOD
      WAVENUMTMP = 0.1D0
1276        FK = GRAV*WAVENUMTMP*DTANH(WAVENUMTMP*DEPTH)-SEGMA**2.0D0

          IF (DABS(FK).GT.1.0e-7) THEN
            FKDIF=GRAV*WAVENUMTMP*DEPTH*(1.0D0-DTANH(WAVENUMTMP*DEPTH)
     +              **2.0D0)+GRAV*DTANH(WAVENUMTMP*DEPTH)
            WAVENUMTMP = WAVENUMTMP-FK/FKDIF
            GOTO 1276
          ENDIF

          WKZ = WAVENUMTMP
      END SUBROUTINE DISPERSION
c------------------------------------------------------------------------


C     #VEG07#####################  SUBROUTINE DVEG  ########################
C
C     This subroutine calculates DVEG due to vegetation
C     using Mendez and Losada (2004), Chen and Zhao (2012)
C
      SUBROUTINE DVEG(cg,J, L, WHRMS, D)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NL=100,NFR=500,NNZ=50,NSPECTRUM=5000)
C     NFR=maximum number of frequency beams for JONSWAP spectrum

      DIMENSION FREQ(NFR),WNUM(NFR),EJONSPEC(NFR),SDSCZ(NFR)
      DIMENSION ZCZ(NNZ),URMSCZ(NNZ)

C     D = water depth
C     WHRMS = RMS wave height

C     submerged vegetation
      EFFVEGH = cg%VEGD(J,L)
C     emergent vegetation
      IF(EFFVEGH.GT.D) EFFVEGH=D

C     cg%IVEG=0 for no vegetation or vegetation represented by increased
C     cg%IVEG=1 for vegataion whose density, width, height and root depth
C     cg%IVEG=2 for vegatation whose constant density, width and height
C     lzhu commented:
c     cg%IVEG=3 for vegetation with density, width, height and root depth
c            specified in input file as cg%IVEG=1, AND allowing to choose
c            different dissipation and phase-averaged drag models by
c            specifying cg%IDISS and cg%IFV
C     cg%IDISS=1: Mendez and Losada (2004)
C     cg%IDISS=2: Chen and Zhao (2012) with JONSWAP spectrum
C     cg%IDISS=3: Chen and Zhao (2012) with measured Se

      IF(cg%IDISS.EQ.1) THEN
        cg%DVEGSTA(J) = (0.5/DSQRT(cg%PI)/cg%GRAV)*cg%VEGCD*
     +            cg%VEGB(J,L)*cg%VEGN(J,L)*
     +            (0.25*cg%WKP*cg%GRAV*cg%WT(J)/cg%PI)**3 *
     +            (DSINH(cg%WKP*EFFVEGH)**3.0 + 3.0*
     +            DSINH(cg%WKP*EFFVEGH)) *
     +            WHRMS**3 /
     +            (3.0*cg%WKP*DCOSH(cg%WKP*D)**3.0)
      ELSEIF(cg%IDISS.EQ.2) THEN
C       Get jonswap spectrum
        GAM = 3.3
        SA = 0.07
        SB = 0.09
        NFREQ = NINT(cg%FREQNUM)

        FREQPEAK = cg%TWOPI/cg%WT(J)
        DFREQ = (cg%FREQMAX-cg%FREQMIN)/NFREQ

        DO 516 IC = 1, NFREQ
          FREQ(IC) = cg%FREQMIN + 0.5*DFREQ + (IC-1)*DFREQ

          PERWAVE    = 2.0*cg%PI/FREQ(IC)
          SEGMA      = FREQ(IC)
          CELERITY   = DSQRT(cg%GRAV*D)
          WAVELENGTH = CELERITY*PERWAVE
          WAVENUM    = cg%TWOPI/WAVELENGTH

76        FK = cg%GRAV*WAVENUM*DTANH(WAVENUM*D)-SEGMA**2.0

          IF (DABS(FK).GT.1.0e-7) THEN
            FKDIF = cg%GRAV*WAVENUM*D*(1.0-DTANH(WAVENUM*D)**2.0)+
     +              cg%GRAV*DTANH(WAVENUM*D)
            WAVENUM = WAVENUM-FK/FKDIF
            GOTO 76
          ENDIF

          WNUM(IC) = WAVENUM

          IF(FREQ(IC).LT.FREQPEAK) THEN
            EJONSPEC(IC) = cg%GRAV**2.0/FREQ(IC)**5.0*
     +                  DEXP(-1.25*(FREQPEAK/FREQ(IC))**4.0)*
     +                  GAM**DEXP(-0.5*(FREQ(IC)/FREQPEAK-1.0)**2.0/
     +                            SA**2.0)
          ELSE
            EJONSPEC(IC) = cg%GRAV**2.0/FREQ(IC)**5.0*
     +                DEXP(-1.25*(FREQPEAK/FREQ(IC))**4.D0)*
     +                GAM**DEXP(-0.5*(FREQ(IC)/FREQPEAK-1.D0)**2.D0/
     +                             SB**2.D0)
          ENDIF

C         For TMA Spectrum
C         EJONSPEC(IC) = EJONSPEC(IC)*DTANH(WNUM(IC)*D)**2.0/
C     +            (1.0+2.0*WNUM(IC)*D/DSINH(2.*WNUM(IC)*D))
516     CONTINUE

C       For Rayleigh distribution, Hrms^2 = 8sigma^2=8m0, Hm0^2=16m0
C       (Hm0^2=16m0, Hm0^2 = 2Hrms^2, Hm0 = sqrt(2)Hrms)
        HM0=DSQRT(2.D0)*WHRMS

C       Get normalization factor AJON, to make sure that
C       m0=Hm0**2/16=int S(f)df
        SUMINT = 0.0
        DO 526 IIFR = 1,NFREQ
          SUMINT = SUMINT+EJONSPEC(IIFR)*DFREQ
526     CONTINUE

        AJON = HM0**2.0/16.0/SUMINT

C       Normalize EJONSPEC with AJON
        DO 536 IIFR = 1,NFREQ
           EJONSPEC(IIFR) = EJONSPEC(IIFR)*AJON
536     CONTINUE

C       Compute Urms(z)
C       Get Z distribution first
        DZCZ = EFFVEGH/(NNZ*1.D0)
        DO 546 IZ = 1,NNZ
           ZCZ(IZ)   =  -D + 0.5*DZCZ + (IZ-1)*DZCZ
           SUMINT = 0.0
           DO 556 IIFR = 1,NFREQ
              TMP1      = DCOSH(WNUM(IIFR)*(D+ZCZ(IZ)))**2.0
              TMP2      = DSINH(WNUM(IIFR)*D)**2.0
              ETMP      = FREQ(IIFR)**2.0*TMP1*
     +                    EJONSPEC(IIFR) / TMP2
              SUMINT    = SUMINT + ETMP*DFREQ
556        CONTINUE
           URMSCZ(IZ)   = DSQRT(2.0*SUMINT)
546     CONTINUE

C       Get Sdx: spectral distribution of dissipation
C       i.e. Eq. (11) in Chen and Zhao (2012)
        DO 566 IIFR = 1,NFREQ
C          Get int(Urms*coshh^2(k(h+z))) in terms of z first
           SUMINT = 0.0
           DO 576 IZ = 1,NNZ
              TMP1   = DCOSH(WNUM(IIFR)*(D+ZCZ(IZ)))**2.0
              SUMINT = SUMINT + URMSCZ(IZ)*TMP1*DZCZ
576        CONTINUE

           SDSCZ(IIFR) =(0.5*cg%VEGCD*cg%VEGB(J,L)*
     +                  cg%VEGN(J,L)/cg%GRAV) *
     +                  (FREQ(IIFR)/DSINH(WNUM(IIFR)*D))**2.0 *
     +                  SUMINT * EJONSPEC(IIFR)
566     CONTINUE

        SUMINT = 0.0
        DO 586 IIFR = 1,NFREQ
           SUMINT = SUMINT + SDSCZ(IIFR)*DFREQ
586     CONTINUE
        cg%DVEGSTA(J) = SUMINT


      ELSEIF(cg%IDISS.EQ.3) THEN
        DFREQ = cg%VMEASOMEG(2) - cg%VMEASOMEG(1)
        NFREQ = cg%NMEASSPEC
C       Compute Urms(z)
        DZCZ = EFFVEGH/(NNZ*1.D0)
        DO 5461 IZ = 1,NNZ
           ZCZ(IZ)   =  -D + 0.5*DZCZ + (IZ-1)*DZCZ

           SUMINT = 0.0
           DO 5561 IIFR = 1,NFREQ
              TMP1      = DCOSH(cg%VMEASWNUM(IIFR)*(D+ZCZ(IZ)))**2.0
              TMP2      = DSINH(cg%VMEASWNUM(IIFR)*D)**2.0
              ETMP      = cg%VMEASOMEG(IIFR)**2.0*TMP1*
     +                    cg%VMEASSE(IIFR) / TMP2
              IF (IIFR.EQ.1.OR.IIFR.EQ.NFREQ) THEN
                 SUMINT    = SUMINT + ETMP*DFREQ*0.5
              ELSE
                 SUMINT    = SUMINT + ETMP*DFREQ
              ENDIF
5561      CONTINUE
          URMSCZ(IZ)   = DSQRT(2.0*SUMINT)
5461    CONTINUE

C       Get Sdx: spectral distribution of dissipation
C       i.e. Eq. (11) in Chen and Zhao (2012)
        DO 5661 IIFR = 1,NFREQ
C          Get int(Urms*coshh^2(k(h+z))) in terms of z first
           SUMINT = 0.0
           DO 5761 IZ = 1,NNZ
              TMP1   = DCOSH(cg%VMEASWNUM(IIFR)*(D+ZCZ(IZ)))**2.0
              SUMINT = SUMINT + URMSCZ(IZ)*TMP1*DZCZ
5761        CONTINUE

           SDSCZ(IIFR) =(0.5*cg%VEGCD*cg%VEGB(J,L)*
     +     cg%VEGN(J,L)/cg%GRAV) *
     +     (cg%VMEASOMEG(IIFR)/DSINH(cg%VMEASWNUM(IIFR)*D))**2.0 *
     +     SUMINT * cg%VMEASSE(IIFR)
5661    CONTINUE

        SUMINT = 0.0
        DO 5861 IIFR = 1,NFREQ
           SUMINT = SUMINT + SDSCZ(IIFR)*DFREQ
5861     CONTINUE
        cg%DVEGSTA(J) = SUMINT

       ENDIF

      RETURN
      END SUBROUTINE DVEG

C     -VEG07-----------------  END OF SUBROUTINE DVEG  ---------------------


C     #22############################ SUBROUTINE EROSON ###########################
C
C     This subroutine computes erosion of grassed dike at time level
C     ITIME and along cross-shore line L if IPROFL=2 and ICLAY=0
C     For ICLAY=1 and IPROFL=1, exposed clay erosion is computed
C
      SUBROUTINE EROSON(cg,ITIME,L,IEND)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NB=30000,NL=100)
      DIMENSION GRS1(NN,NL),GRS2(NN,NL),GRS3(NN,NL),GRS4(NN,NL),
     +   GRS5(NN,NL),FBA3(NN,NL),DFSWD(NN),BSF(NN),DUMVEC(NN)
C
C
C     Dike Erosion efficiencies eB and ef associated with wave breaking
C     and bottom friction are specified in Subr.2 INPUT
      DATA DELEM,SCP/0.05D0,1.2D0/
C     where DELEM=maximum allowable dike erosion increment (m) and SCP=
C     maximum slope of eroded dike clay soil
C
C     The following parameters are invariant with cg%TIME
      IF(cg%TIME.EQ.0.D0)THEN
        DO 100 J=1,cg%JMAX(L)
          FBA3(J,L)=cg%FB2(J,L)*DSQRT(cg%GRAV)*cg%AWD**3.D0
          IF(cg%ICLAY.EQ.1) THEN
              cg%EPCLAY(J,L)=0.D0
              BSF(J)=1.0D0
          ELSE
             GRS3(J,L)=cg%GRAV/cg%GRSRD(J,L)
             GRS4(J,L)=0.5D0*cg%GRSD(J,L)*(cg%GRSR(J,L)-cg%GRSRD(J,L))/
     +         cg%GRSRD(J,L)
             GRS5(J,L)=0.5D0*cg%GRSD(J,L)*(cg%GRSR(J,L)+cg%GRSRD(J,L))/
     +         cg%GRAV
              IF(cg%GRSD(J,L).LE.0.D0) THEN
                  GRS1(J,L)=0.D0
                  GRS2(J,L)=0.D0
              ELSE
                  DUM=cg%GRSR(J,L)-cg%GRSRD(J,L)
                  GRS1(J,L)=cg%GRSD(J,L)*cg%GRSR(J,L)/DUM
                  GRS2(J,L)=2.D0*cg%GRAV*DUM/cg%GRSD(J,L)/
     +                      cg%GRSR(J,L)**2.D0
              ENDIF
              cg%DSUM(J)=0.D0
          ENDIF
 100    CONTINUE
      ENDIF
C
C     BSF(J)=bottom slope function for dike erosion
C     where computed values are smoothed to obtain BSF(J)
      IF(cg%ICLAY.EQ.0) THEN
          DO 150 J=1,cg%JDRY
              ASB=DABS(cg%BSLOPE(J,L))
              DUM=ASB/SCP
              IF(DUM.GE.0.9D0)THEN
                  DUMVEC(J)=10.D0
              ELSE
                  DUMVEC(J)=1.D0/(1.D0-DUM)
              ENDIF
 150  CONTINUE
      CALL SMOOTH(cg,cg%JDRY,DUMVEC,BSF)
      ENDIF
C
C     cg%DSTA(J)=dike erosion forcing at given cg%TIME
C     cg%DSUM(J)=value of cg%DSTA(J) integrated from cg%TIME=0.0
C     cg%DSTA(J,L) is computed for wet nodes (J=1 to cg%JR) and for wet and
C     dry nodes (J=cg%JWD to cg%JDRY) separately
      DO 200 J=1,cg%JR
        IF(cg%IROLL.EQ.0) THEN
          cg%DSTA(J)=cg%DEEB*cg%DBSTA(J)+cg%DEEF*cg%DFSTA(J)
        ELSE
          cg%DSTA(J)=cg%DEEB*cg%RBETA(J)*cg%RQ(J)+cg%DEEF*cg%DFSTA(J)
        ENDIF
        cg%DSTA(J)=cg%DSTA(J)*BSF(J)
 200  CONTINUE
      ED=1.D0
      DO 210 J=cg%JWD,cg%JDRY
        DUM=cg%AQWD*cg%H(J)*DSQRT(cg%GRAV*cg%H(J)/cg%PWET(J))
        IF(DUM.LT.1.D-6) THEN
          RS=0.D0
        ELSE
          RS=cg%CWD*(cg%QO(L)-DUM)/DUM
        ENDIF
        DFSWD(J)=ED*FBA3(J,L)*cg%H(J)*DSQRT(cg%H(J)/cg%PWET(J))*
     +           GFDWD(RS)
        DFSWD(J)=DFSWD(J)*BSF(J)
C     Function GFDWD(R) is specified below this subroutine
        IF(J.EQ.cg%JWD)THEN
          ED=cg%DSTA(J)/DFSWD(J)
          DFSWD(J)=cg%DSTA(J)
        ENDIF
 210  CONTINUE
C
C     Connect cg%DSTA(J) and DFSWD(J) and smooth connected cg%DSTA(J)
C     using Subr.17 TRANWD, Subr.14 SMOOTH and Subr.15 EXTRAPO
      IF(cg%JDRY.GT.cg%JR) THEN
        CALL TRANWD(cg%DSTA,cg%JR,DFSWD,cg%JWD,cg%JDRY)
      ELSE
        cg%JDRY=cg%JR
      ENDIF
      DUMVEC=cg%DSTA
      CALL SMOOTH(cg,cg%JDRY,DUMVEC,cg%DSTA)
      IF(cg%JDRY.LT.cg%JMAX(L)) THEN
        JDRY1=cg%JDRY+1
        CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%DSTA)
      ENDIF
C
C     Find cg%TIME step size cg%DELT based on DELEM in DATA for cg%TIME
C     marching computation of numerical stability if cg%ICLAY=0
      IF(cg%ICLAY.EQ.0) THEN
      DMAX=cg%DSTA(1)*GRS3(1,L)
      DO 300 J=2,cg%JMAX(L)
        DUM=cg%DSTA(J)*GRS3(J,L)
        IF(DUM.GT.DMAX) DMAX=DUM
 300  CONTINUE
      IF(DMAX.LT.1.D-6) DMAX=1.D-6
      cg%DELT=DELEM/DMAX
      IDUM=ITIME+1
      DUM=(cg%TIMEBC(IDUM)-cg%TIMEBC(ITIME))/2.D0
      IF(cg%DELT.GT.DUM) cg%DELT=DUM
      DUM=cg%TIME+cg%DELT
      IF(DUM.GE.cg%TIMEBC(IDUM)) THEN
        cg%DELT=cg%TIMEBC(IDUM)-cg%TIME
        IEND=1
      ENDIF
C     where IEND=1 indicates the end of each ITIME computation
C     in Main Program
C
C     cg%EDIKE(J,L)=downward erosion depth (m) from initial (cg%TIME=0.0)
C     dike surface at cg%TIME=(cg%TIME+cg%DELT) if cg%ICLAY=0
      DO 400 J=1,cg%JMAX(L)
        cg%DSUM(J)=cg%DSUM(J)+cg%DELT*cg%DSTA(J)
        IF(cg%GRSD(J,L).GT.0.D0) THEN
          IF(cg%DSUM(J).LT.GRS5(J,L)) THEN
            cg%EDIKE(J,L)=GRS1(J,L)*(1.D0-DSQRT(1.D0-GRS2(J,L)*
     +                    cg%DSUM(J)))
          ELSE
            cg%EDIKE(J,L)=GRS3(J,L)*cg%DSUM(J)-GRS4(J,L)
          ENDIF
        ELSE
          cg%EDIKE(J,L)=GRS3(J,L)*cg%DSUM(J)
        ENDIF
        cg%ZB(J,L)=cg%ZB0(J,L)-cg%EDIKE(J,L)
 400  CONTINUE
      ENDIF
C
C     ECLAY(J,L)=downward clay erosion depth (m) from initial clay surface
C     below sand layer using cg%DELT computed in Subr. 12 CHANGE if cg%ICLAY=1
      IF(cg%ICLAY.EQ.1) THEN
      DO 500 J=1,cg%JMAX(L)
          IF(cg%HP(J,L).LT.cg%D50) THEN
              DUM=cg%DELT*cg%RCLAY(J,L)*cg%DSTA(J)
              cg%EPCLAY(J,L)=cg%EPCLAY(J,L)+DUM
              cg%ZP(J,L)=cg%ZP0(J,L)-cg%EPCLAY(J,L)
              cg%ZB(J,L)=cg%ZB(J,L)-DUM*cg%FCLAY(J,L)
          ENDIF
 500  CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE EROSON
C
C     -22------------------------- END OF SUBROUTINE EROSON -----------------------


C     #15####################### SUBROUTINE EXTRAPO #######################
C     Extrapolate vector F from node J1 to node J2
C     where values of F at nodes J=1 to (J1-1) are computed
      SUBROUTINE EXTRAPO(cg,J1,J2,F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000)
      DIMENSION F(NN)
C
C     cg%NPE = number of points (computed in Subr.3 BOTTOM) extrapolated
C     to avoid a sudden jump from computed F(J1-1) to zero
      JJ=J1+cg%NPE
      IF(JJ.LE.J2) THEN
      JM = J1-1
      Y= F(JM)
      DELY = Y/DBLE(cg%NPE+1)
      DO 100 J=1,cg%NPE
        F(JM+J) = Y-DELY*DBLE(J)
 100  CONTINUE
      F(JJ:J2) = 0.D0
      ELSE
        IF(J1.LE.J2) F(J1:J2)=0.D0
      ENDIF
C
      RETURN
      END SUBROUTINE EXTRAPO
C     -15---------------- END OF SUBROUTINE EXTRAPO ------------------------

C     *****************************************************************************
C     This function related to dike erosion forcing in wet and dry
C     zone is called from Subr.22 EROSON
      FUNCTION GFDWD(R)
      DOUBLE PRECISION GFDWD,R,TR,R2,R3
      TR=3.D0*R
      R2=R*R
      R3=R2*R
      IF(R.GE.0.D0) THEN
        GFDWD=1.32934D0+TR+2.658681D0*R2+R3
      ELSE
        GFDWD=1.32934D0*(1.D0+2.D0*R2)*(2.D0*ERFCC(R)-1.D0)
     +  -TR-R3+(16.D0*R3+9.D0*R)*DEXP(-R2)
      ENDIF
C     Complementary error function ERFCC below Subr.06 GBXAGF
C
      RETURN
      END FUNCTION GFDWD
C     *****************************************************************************


C     #17####################### SUBROUTINE TRANWD #########################
C     Connect vector F1(J) with J=1 to JR with vector F2(J)
C     with J=JS to JE where JE is not less than JR
C
      SUBROUTINE TRANWD(F1,JR,F2,JS,JE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NN=5000)
      DIMENSION F1(NN),F2(NN)
C
      IF(JR.GE.JS) THEN
        DO 100 J=JS,JR
          F1(J) = 0.5D0*(F1(J)+F2(J))
 100    CONTINUE
        DO 105 J=(JR+1),JE
          F1(J) = F2(J)
 105    CONTINUE
      ENDIF
      IF(JR.LT.JS) THEN
        DSR=DBLE(JS-JR)
        DO 200 J=(JR+1),JS
          DUM=DBLE(JS-J)/DSR
          F1(J)=F1(JR)*DUM+F2(JS)*(1.D0-DUM)
 200    CONTINUE
        DO 205 J=(JS+1),JE
          F1(J)=F2(J)
 205    CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE TRANWD
C     -17---------------------- END OF SUBROUTINE TRANWD ----------------------


C     *****************SUBROUTINE GBXAGF**********************************
C     This subroutine computes GBX and GF for specified CTHETA, USIGT,
C     STHETA and VSIGT for Gaussian variable R
C
      SUBROUTINE GBXAGF(cg,CTHETA,USIGT,STHETA,VSIGT,GBX,GF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER(NL=100)
C
C     For obliquelly incident waves, use approximate equations
      IF(cg%IANGLE.EQ.1) THEN
        RM  = -USIGT*CTHETA - VSIGT*STHETA
        AFM = DABS(VSIGT*CTHETA - USIGT*STHETA)
        DUM = USIGT*USIGT + VSIGT*VSIGT
        GBX = cg%SQRG1*(USIGT - RM*CTHETA)+ USIGT*AFM
        GF  = cg%SQRG2 + (1.D0 + DUM)*AFM + cg%SQRG1*(DUM + 2.D0*RM*RM)
      ENDIF
C
C     For normally incident waves, use analytical
C     expresions involving complementary error function ERFCC below
      IF(cg%IANGLE.EQ.0) THEN
        C1 = 1.D0-ERFCC(USIGT/cg%SQR2)
        C2 = cg%SQRG1*DEXP(-USIGT*USIGT/2.D0)
        C3 = 1.D0 + USIGT*USIGT
        GBX = C3*C1 + C2*USIGT
        GF = USIGT*(C3 + 2.D0)*C1 + (C3 + 1.D0)*C2
      ENDIF
C
      RETURN
      END SUBROUTINE GBXAGF
C
C     -----------------------END OF SUBROUTINE GBXAGF---------------------


C     #24##################### SUBROUTINE INTERP1 ##############################
C
C     This subroutine interpolates
c      REAL FUNCTION INTERP1(NINTERP, XDATA, YDATA, XVAL)
      SUBROUTINE INTERP1(NINTERP, XDATA, YDATA, XVAL, YVAL)

      DOUBLE PRECISION XDATA(NINTERP), YDATA(NINTERP)
      DOUBLE PRECISION XVAL, YVAL
      INTEGER IX, NINTERP


      DO 112 IX = 1,NINTERP-1
         IF (XVAL.GE.XDATA(IX).AND.XVAL.LE.XDATA(IX+1)) THEN
            YVAL = YDATA(IX) +
     +             (XVAL-XDATA(IX)) *
     +             (YDATA(IX+1)-YDATA(IX))/(XDATA(IX+1)-XDATA(IX))
         ENDIF

         IF (XVAL.GT.XDATA(NINTERP)) THEN
            YVAL = YDATA(NINTERP-1) +
     +             (XVAL-XDATA(NINTERP-1)) *
     +             (YDATA(NINTERP)-YDATA(NINTERP-1))
     +             /(XDATA(NINTERP)-XDATA(NINTERP-1))
         ENDIF

         IF (XVAL.LT.XDATA(1)) THEN
            YVAL = YDATA(1) +
     +             (XVAL-XDATA(1)) *
     +             (YDATA(2)-YDATA(1))/(XDATA(2)-XDATA(1))
         ENDIF

112   CONTINUE

      END SUBROUTINE INTERP1
c------------------------------------------------------------------------

C     #05#####################  SUBROUTINE LWAVE  ########################
C
C     This subroutine calculates quantities based on linear wave theory
C
      SUBROUTINE LWAVE(cg,J, L, WD, QDISP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000, NL=100)
C
C
C     ... LINEAR WAVE PARAMETERS
C
C     WD     = mean water depth from Main Program
C     QDISP  = water flux affecting wave period
C     cg%TP     = representative wave period specified as input
C     cg%WKP    = wave number at node J
C     cg%WT(J)  = wave period at node J
C     cg%CP(J)  = phase velocity based on input cg%TP at node J
C     cg%WN(J)  = ratio of group velocity to phase velocity at node J
C
C     Solve linear wave dispersion relation with no current to find cg%WKP
      IF(cg%IWCINT.EQ.0.OR.QDISP.EQ.0.D0) THEN
        D = WD*cg%WKPO
        IF(J.EQ.1) THEN
          X = D/DSQRT(DTANH(D))
        ELSE
          X = cg%WKP*WD
        ENDIF
 10     COTH = 1.D0/DTANH(X)
        XNEW = X - (X-D*COTH)/(1.D0+D*(COTH**2.D0-1.D0))
        IF (DABS(XNEW - X).GT.1.D-7) THEN
          X = XNEW
          GOTO 10
        ENDIF
        AF = cg%TWOPI/cg%TP
C
C     Solve linear wave dispersion relation with current to find cg%WKP
      ELSE
        B = cg%TP*QDISP/cg%TWOPI/WD/WD
        D = WD*cg%WKPO
        IF(J.EQ.1) THEN
          X = D/DSQRT(DTANH(D))
        ELSE
          X = cg%WKP*WD
        ENDIF
 11     COTH = 1.D0/DTANH(X)
        C = 1.D0 - B*X
        F = X - D*C*C*COTH
        FD = 1.D0 + D*C*(2.D0*B*COTH + C*(COTH*COTH - 1.D0))
        XNEW = X - F/FD
        IF (DABS(XNEW - X).GT.1.D-7) THEN
          X = XNEW
          GOTO 11
        ENDIF
        AF = DSQRT(cg%GRAV*XNEW*DTANH(XNEW)/WD)
      ENDIF
C
      cg%WKP = XNEW/WD
      X2 = X*2.D0
      cg%WN(J) = 0.5D0*(1.D0 + X2/DSINH(X2))
      cg%WT(J) = cg%TWOPI/AF
      cg%CP(J) = AF/cg%WKP
      cg%FSX = 2.D0*cg%WN(J) - 0.5D0
      cg%FSY = 0.D0
      cg%FE  = cg%WN(J)*cg%CP(J)*cg%WT(J)
C
C     If cg%IANGLE=0, normally incident waves
      IF(cg%IANGLE.EQ.0) THEN
        cg%STHETA(J) = 0.D0
        cg%CTHETA(J) = 1.D0
        GOTO 100
      ENDIF
C
C     Otherwise, compute wave cg%ANGLE THETA in radians at node J using
C     Snell's Law where cg%ANGLE = incident wave cg%ANGLE in degrees at
C     node J=1, cg%AGLINE(L) = cg%ANGLE of cross-shore line L, and cg%WKPSIN = constant
C     Wave cg%ANGLE from shorenormal is limited to range of -180 to 180 degrees
C     before imposing range of -80 and 80 degrees
C
      IF(J.EQ.1) THEN
        DUM=cg%ANGLE-cg%AGLINE(L)
        IF(DUM.GT.180.D0) DUM=DUM-360.D0
        IF(DUM.LT.-180.D0) DUM=DUM+360.D0
        IF(DUM.GT.80.D0) DUM=80.D0
        IF(DUM.LT.-80.D0) DUM=-80.D0
        THETA = DUM*cg%PI/180.D0
        cg%STHETA(1) = DSIN(THETA)
        cg%CTHETA(1) = DCOS(THETA)
        cg%WKPSIN = cg%WKP*cg%STHETA(1)
      ELSE
        cg%STHETA(J) = cg%WKPSIN/cg%WKP
        THETA = DASIN(cg%STHETA(J))
        cg%CTHETA(J) = DCOS(THETA)
      ENDIF
C
      cg%FSX = cg%FSX - cg%WN(J)*cg%STHETA(J)*cg%STHETA(J)
      cg%FSY = cg%WN(J)*cg%STHETA(J)*cg%CTHETA(J)
      cg%FE = cg%FE*cg%CTHETA(J)
C
 100  IF(cg%IWCINT.EQ.1) cg%FE=cg%FE+cg%WT(J)*cg%QWX/WD
C
C     Compute cg%RX, cg%RY and cg%RE related to roller momentum and energy fluxes
C     as well as cg%RBETA =wave-front slope of roller with cg%RBZERO = 0.1
      IF(cg%IROLL.EQ.1) THEN
        IF(cg%IANGLE.EQ.0) THEN
          cg%RX(J)=cg%CP(J)/cg%GRAV
          cg%RE(J)=cg%RX(J)*cg%CP(J)
        ELSE
          DUM=cg%CP(J)*cg%CTHETA(J)/cg%GRAV
          cg%RX(J)=DUM*cg%CTHETA(J)
          cg%RY(J)=DUM*cg%STHETA(J)
          cg%RE(J)=DUM*cg%CP(J)
        ENDIF
        cg%RBETA(J)=cg%RBZERO
        IF(cg%BSLOPE(J,L).GT.0.D0) cg%RBETA(J)=cg%RBETA(J)+
     +          cg%BSLOPE(J,L)*cg%CTHETA(J)
      ENDIF
C
      RETURN
      END SUBROUTINE LWAVE
C
C     -05-----------------  END OF SUBROUTINE  LWAVE ---------------------


C     #09#####################  SUBROUTINE POFLOW  ########################
C
C     This subroutine computes mean and standard deviation of
C     porous flow velocity and wave energy dissipation rate
C     DPSTA for given PKHSIG and DEDX at node J in the wet zone
C
      SUBROUTINE POFLOW(cg,J, L, PKHSIG, DEDX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
C
      PARAMETER(NN=5000, NL=100)
C
C
C     For porous layer thickness cg%HP(J,L)=0.0, no velocity and dissipation
      IF(cg%HP(J,L).EQ.0.D0) THEN
        cg%UPMEAN(J) = 0.D0
        cg%UPSTD(J) = 0.D0
        cg%DPSTA(J) = 0.D0
      ENDIF
C
      IF(cg%HP(J,L).GT.0.D0) THEN
        A = 1.9D0*cg%BESTA1
        B2 = cg%BESTA2/cg%WT(J)
        B = cg%ALSTA + 1.9D0*B2
        cg%UPSTD(J) = 0.5D0*(DSQRT(B*B+4.D0*A*PKHSIG)-B)/A
        A = cg%SQRG1*(B2+cg%BESTA1*cg%UPSTD(J))
        C = cg%CTHETA(J)*cg%CTHETA(J)
        cg%UPMEAN(J) = -DEDX/(cg%ALSTA+A*(1.D0+C))
C
C     To reduce numerical oscillations of cg%UPMEAN(J), adjust
        RATIO = cg%UPMEAN(J)/cg%UPSTD(J)
        IF(RATIO.GT.0.5D0) cg%UPMEAN(J)=0.5D0*cg%UPSTD(J)
        IF(RATIO.LT.-0.5D0) cg%UPMEAN(J)=-0.5D0*cg%UPSTD(J)
        cg%QP(J)=cg%UPMEAN(J)*cg%HP(J,L)
C
        A2 = cg%UPMEAN(J)*cg%UPMEAN(J)
        B2 = cg%UPSTD(J)*cg%UPSTD(J)
        cg%DPSTA(J) = cg%HP(J,L)*(cg%ALSTA*(A2+B2)+A*
     +          (2.D0*B2+A2*(1.D0+2.D0*C)))
C
      ENDIF
C
      RETURN
      END SUBROUTINE POFLOW
C     ------------------  END OF SUBROUTINE POFLOW  ---------------------


C     #20##################### SUBROUTINE PONDED ##############################
C
C     This subroutine computes ponded water level and zone if IPOND=1
C
      SUBROUTINE PONDED(cg,L)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NL=100)
C
C
C     Compute the following quantities for known bottom
C     profile cg%ZB(J,L) above datum at cg%XB(J) for node J for line L
C     cg%ZW = ponded water level at cg%TIME=cg%TIME
C     cg%JCREST(L) = ridge crest node landward of SWL node cg%JSWL
C     cg%JXW = seaward end node of ponded water zone
C     cg%JX2 = landward end node of ponded water zone
C
C     For cg%TIME=0.0, cg%ZW=SWLBC(1) as specified in Subr.2 INPUT
C     For cg%TIME>0, compute cg%ZW at present cg%TIME level
      IF(cg%TIME.GT.0.D0) THEN
        IF(cg%JX2.GT.cg%JXW) THEN
          cg%ZW=cg%ZW+cg%DELT*(cg%QO(L)-cg%QM)/(cg%XB(cg%JX2)-
     +          cg%XB(cg%JXW))
        ENDIF
        IF(cg%ZW.GT.cg%ZB(cg%JMAX(L),L)) cg%ZW=cg%ZB(cg%JMAX(L),L)
      ENDIF
C
C     cg%NOPOND=0 for ponded water in runnel
C     cg%NOPOND=1 for submerged ridge and runnel seaward
C     of node cg%JSWL(L) or dry runnel with no ponded water
      JRUN=cg%JMAX(L)
      JPEAK=cg%JMAX(L)
      DO 100 J=(cg%JSWL(L)+1),(cg%JMAX(L)-1)
      IF(cg%ZB(J-1,L).GE.cg%ZB(J,L).AND.cg%ZB(J,L).LT.cg%ZB(J+1,L)) THEN
        IF(cg%ZB(J,L).LT.cg%ZB(JRUN,L).AND.cg%ZW.GT.cg%ZB(J,L)) JRUN=J
      ENDIF
      IF(cg%ZB(J,L).GE.cg%ZB(JPEAK,L)) JPEAK=J
 100  CONTINUE
      IF(JRUN.EQ.cg%JMAX(L)) THEN
        cg%NOPOND=1
        cg%JCREST(L)=JPEAK
        cg%RCREST(L)=cg%ZB(cg%JCREST(L),L)
        cg%JXW=cg%JSWL(L)
        cg%JX2=cg%JMAX(L)
        cg%ZW=cg%ZB(cg%JSWL(L),L)
        GOTO 200
      ELSE
        cg%NOPOND=0
      ENDIF
C     For cg%NOPOND=1, node cg%JCREST(J) is highest bottom elevation and
C     water level cg%ZW is set to be still water level.
C
C     cg%JCREST(L) = node of ridge crest located between
C     nodes cg%JSWL(L) and JRUN if cg%NOPOND=0
      cg%JCREST(L)=cg%JSWL(L)
      DO 110 J=(cg%JSWL(L)+1),(JRUN-1)
      IF(cg%ZB(J-1,L).LE.cg%ZB(J,L).AND.cg%ZB(J,L).GT.cg%ZB(J+1,L)) THEN
        IF(cg%ZB(J,L).GT.cg%ZB(cg%JCREST(L),L)) cg%JCREST(L)=J
      ENDIF
 110  CONTINUE
C
      IF(cg%JCREST(L).EQ.cg%JSWL(L)) THEN
        cg%NOPOND=1
        cg%JCREST(L)=JPEAK
        cg%RCREST(L)=cg%ZB(cg%JCREST(L),L)
        cg%JXW=cg%JSWL(L)
        cg%JX2=cg%JMAX(L)
        cg%ZW=cg%ZB(cg%JSWL(L),L)
        GOTO 200
      ENDIF
      cg%RCREST(L)=cg%ZB(cg%JCREST(L),L)
C     If ponded water in runnel is full landward of ridge
C     crest, lower cg%ZW to ridge crest elevation
      IF(cg%ZW.GT.cg%ZB(cg%JCREST(L),L)) cg%ZW=cg%ZB(cg%JCREST(L),L)
C
C     Find nodes cg%JXW and cg%JX2 at water level cg%ZW
      J=cg%JCREST(L)
 120  IF(cg%ZB(J,L).LE.cg%ZW) THEN
        cg%JXW=J
        GOTO 121
      ELSE
        J=J+1
        IF(J.EQ.JRUN) THEN
          cg%JXW=JRUN-1
        GOTO 121
        ENDIF
        GOTO 120
      ENDIF
 121  J=JRUN
 125  IF(cg%ZB(J,L).GT.cg%ZW) THEN
        cg%JX2=J-1
        GOTO 200
      ELSE
        J=J+1
        IF(J.EQ.cg%JMAX(L)) THEN
          cg%JX2=cg%JMAX(L)
          GOTO 200
        ENDIF
        GOTO 125
      ENDIF
C
 200  CONTINUE
      RETURN
      END SUBROUTINE PONDED
C
C     -20------------------------- END OF SUBROUTINE PONDED -----------------------


C     #18########################## SUBROUTINE PROBWD ##########################
C     Compute bedload probability PBWD(J) and suspended load
C     probability PSWD(J) in wet and dry zone in Subr.11 SEDTRA
C
      SUBROUTINE PROBWD(PW,A,US,UC,P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF(DABS(US).LE.UC) THEN
        P=PW*DEXP(-A*(UC-US)**2)
      ELSE
        IF(US.GT.UC) THEN
          P=PW
        ELSE
          P=PW*(1.D0-DEXP(-A*(UC+US)**2)+DEXP(-A*(UC-US)**2))
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE PROBWD
C     -18----------------------- END OF SUBROUTINE PROBWD ---------------------


C     #11#####################  SUBROUTINE SEDTRA  ########################
C
C     This subr. calculates cross-shore and longshore sediment transport
C
      SUBROUTINE SEDTRA(cg,L)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NB=30000,NL=100)
      DIMENSION QRAW(NN),GSLRAW(NN),ASLRAW(NN),ASLOPE(NN),RS(NN),RB(NN),
     +   PBWD(NN),PSWD(NN),VSWD(NN),QSXWD(NN),QBXWD(NN),QRAWD(NN),
     +   HDIP(NN),QSYWD(NN),QBYWD(NN)
C
C
C     GSLMAX = Maximum absolute value of cg%GSLOPE function
      DATA GSLMAX,BQCOEFF  /10.D0,8.D0/
C
C.....Cross-Shore and Longshore Sediment Transport at Node J
C     RB(J) = Sediment movement initiation parameter
C     RS(J) = Sediment suspension initiation parameter
C     cg%PB(J) = bedload probability
C     cg%PS(J) = suspended load probability
C     cg%VS(J) = suspended sediment volume per unit area (m)
C     cg%GSLOPE(J) = bed slope correction for cg%QBX(J)
C     ASLOPE(J) = bed slope correction for suspended load parameter cg%SLP
C     cg%QBX(J)= Cross-shore  bedload transport rate per unit width (m*m/s)
C     cg%QBY(J)= Longshore  bedload transport rate per unit width (m*m/s)
C     BRF   = Bedload reduction factor for hard bottom (cg%ISEDAV=1)
C     cg%QSX(J)= Cross-shore suspended sediment transport rate (m*m/s)
C     cg%QSY(J)= Longshore suspended sediment transport rate (m*m/s)
C     cg%Q(J)  = total cross-shore sedimet transport rate including void
C     (m*m/s) used for beach profile change computation
C
      IF(cg%TIME.EQ.0.D0) THEN
        cg%BSLOP1 = -cg%TANPHI*(GSLMAX-1.D0)/GSLMAX
        cg%BSLOP2 =  cg%TANPHI*(GSLMAX+1.D0)/(GSLMAX+2.D0)
        IGMILD=0
        IF(cg%IPERM.EQ.1.AND.cg%JSWL(L).LT.cg%JMAX(L)) THEN
          DUM=0.5D0*cg%TANPHI
          IF(cg%BSLOPE(cg%JSWL(L),L).LT.DUM.AND.cg%IWTRAN.EQ.0) IGMILD=1
          IF(cg%HP(cg%JSWL(L),L).EQ.0.D0) IGMILD=0
        ENDIF
C     where input bedload parameter is increased in the surf zone in the
C     following if IGMILD=1 (based on two gravel tests MH and MB only)
      ENDIF
      IF(cg%IVWALL(L).EQ.2) THEN
        JDUM=cg%JMAX(L)
        cg%BSLOPE(JDUM,L)=0.D0
        cg%BSLOPE(JDUM-1,L)=(cg%ZB(JDUM-1,L)-cg%ZB(JDUM-2,L))/cg%DX
      ENDIF
C
      DO 100 J=1,cg%JMAX(L)
        IF(cg%BSLOPE(J,L).LT.0.D0) THEN
          IF(cg%BSLOPE(J,L).GT.cg%BSLOP1) THEN
            GSLRAW(J) = cg%TANPHI/(cg%TANPHI + cg%BSLOPE(J,L))
          ELSE
            GSLRAW(J) = GSLMAX
          ENDIF
        ELSE
          IF(cg%BSLOPE(J,L).LT.cg%BSLOP2) THEN
            GSLRAW(J) = (cg%TANPHI - 2.D0*cg%BSLOPE(J,L))/
     +          (cg%TANPHI-cg%BSLOPE(J,L))
          ELSE
            GSLRAW(J) = -GSLMAX
          ENDIF
          IF(IGMILD.EQ.1) THEN
            IF(GSLRAW(J).LT.0.D0) GSLRAW(J)=0.D0
          ENDIF
        ENDIF
        ASLRAW(J) = cg%SLP
C     Add bottom slope effect to suspended load parameter
      IF(cg%BSLOPE(J,L).GT.0.D0) ASLRAW(J)=cg%SLP+
     +          (cg%BSLOPE(J,L)/cg%TANPHI)
     +  **0.5D0
C
C     Vegetation effect is included in cg%DFSTA(J) with J=1,2,...,cg%JR
C     for energy dissipation rate due to bottom friction and vegetation
C     Assume no vegetation effect on sediment transport if cg%IVEG=2
C     IF(J.LE.cg%JR.AND.cg%IVEG.GE.1) THEN
C       DUM=cg%VEGH(J,L)
C       IF(DUM.GT.cg%H(J)) DUM=cg%H(J)
C       VEGCV=1.D0+DUM*cg%VEGFB(J,L)
C       cg%DFSTA(J)=cg%DFSTA(J)/VEGCV
C     ENDIF
C     Vegetation effect is removed from cg%DFSTA(J) above
C
 100  CONTINUE
C
C     Smoothing cg%GSLOPE before cg%Q is computed in Subr.14 SMOOTH
      JMAXL=cg%JMAX(L)
      CALL SMOOTH(cg,JMAXL, GSLRAW, cg%GSLOPE)
      CALL SMOOTH(cg,JMAXL, ASLRAW, ASLOPE)
C
C     Sediment transport rates are computed for normally incident waves
C     in wet zone (cg%IANGLE=0); wet and dry zone (cg%IOVER=1) for cg%IANGLE=0
C     and 1; and obliquelly incident waves in wet zone (cg%IANGLE=1)
      IF(cg%IANGLE.EQ.1) GOTO 888
C
C.....Normally Incident Waves in wet zone...............................
      IF(cg%IANGLE.EQ.0) THEN
        DO 110 J = 1,cg%JR
          IF(cg%D50.LT.cg%CSEDIA) THEN
            RB(J) = DSQRT(cg%GSD50S/cg%FB2(J,L))/cg%USTD(J)
          ELSE
            RB(J)=cg%GSD50S/cg%USTD(J)
          ENDIF
          RS(J) = cg%WF/cg%USTD(J)/cg%FB2(J,L)**0.3333D0
          US = cg%USTA(J)
          cg%PB(J)=0.5D0*(ERFCC((RB(J)+US)/cg%SQR2 )+
     +          ERFCC((RB(J)-US)/cg%SQR2))
          cg%PS(J)=0.5D0*(ERFCC((RS(J)+US)/cg%SQR2 )+
     +          ERFCC((RS(J)-US)/cg%SQR2))
          IF(cg%PS(J).GT.cg%PB(J)) cg%PS(J) = cg%PB(J)
          IF(cg%IROLL.EQ.0) THEN
            cg%VS(J) = cg%PS(J)*(cg%EFFF*cg%DFSTA(J) +
     +          cg%EFFB*cg%DBSTA(J))/cg%WFSGM1
          ELSE
            cg%VS(J) = cg%PS(J)*(cg%EFFF*cg%DFSTA(J) +
     +          cg%EFFB*cg%RBETA(J)*cg%RQ(J))/cg%WFSGM1
          ENDIF
          cg%VS(J) = cg%VS(J)*DSQRT(1.D0+cg%BSLOPE(J,L)*cg%BSLOPE(J,L))
          BQ=cg%BLD
C     Input bedload parameter in Subr.2 INPUT is adjusted to account
C     for cg%QBREAK=fraction(0.0 to 1.0) of breaking waves.
C     Put "C" in front of the next line for no adjustment
          IF(cg%D50.LT.cg%CSEDIA)BQ=BQ*(0.5D0+cg%QBREAK(J))
          IF(IGMILD.EQ.1) THEN
            BQ=cg%BLD*(1.D0+BQCOEFF*cg%QBREAK(J))
          ENDIF
C     BDJ added 2012-10-23
            DECAYL = MIN(cg%XB(cg%JSWL(L))/4.D0,2.D0*cg%TP*cg%CP(1)) ! The decay length
            JDECAY = NINT(DECAYL/cg%DX)! index of decay intrusion length
C     end BDJ added 2012-10-23
          cg%QBX(J) = BQ*cg%PB(J)*cg%GSLOPE(J)*cg%USTD(J)**3
          IF(cg%ISEDAV.GE.1.OR.cg%ISTSAN.EQ.1) THEN
            IF(cg%ISEDAV.GE.1) THEN
               DUM=cg%HP(J,L)
               IF(cg%ISEDAV.EQ.2) THEN
                  DUM=cg%ZB(J,L)-cg%ZMESH(J,L)
                  IF(DUM.LT.0.D0) DUM=0.D0
               ENDIF
               IF(DUM.GE.cg%D50) THEN
                  BRF=1.D0
               ELSE
                  BRF=(DUM/cg%D50)**cg%BEDLM
               ENDIF
            ELSE
               BRF=DEXP(-cg%CPSTON*cg%HP(J,L)/cg%D50)
            ENDIF
            cg%VS(J)=BRF*cg%VS(J)
            cg%QBX(J)=BRF*cg%QBX(J)
          ENDIF
          cg%QSX(J) = ASLOPE(J)*cg%UMEAN(J)*cg%VS(J)
C     Add onshore suspended sediment transport due to wave overtopping
          IF(cg%IOVER.EQ.1) THEN
            DUM = cg%H(J)
            IF(DUM.LT.cg%HWDMIN) DUM = cg%HWDMIN
            AO=cg%SLPOT
            DUMQ=cg%QO(L)
            cg%QSX(J)=cg%QSX(J)+AO*cg%VS(J)*DUMQ/DUM
          ENDIF
          QRAW(J) = (cg%QBX(J) + cg%QSX(J))/cg%SPORO1
 110    CONTINUE
C
C     BDJ added on 2012-10-24
        cg%QSX(1:JDECAY) = cg%QSX(JDECAY)
        cg%QBX(1:JDECAY) = cg%QBX(JDECAY)
        QRAW(1:JDECAY) = QRAW(JDECAY)
C     end BDJ added on 2012-10-24
C
C     If cg%IOVER=0 or cg%JDRY.LE.cg%JR, no wet and dry zone and use scarping formula
C     If cg%IOVER=1, compute sediment transport in wet and dry zone
        IF(cg%IOVER.EQ.0.OR.cg%JDRY.LE.cg%JR) THEN
C
C     Linear extrapolation for scarped slope exceeding cg%TANPHI
C     only if QRAW(cg%JR) is offshore
          JR1 = cg%JR+1
          JE = JR1
          IF(QRAW(cg%JR).LT.0.D0) THEN
 102        IF(cg%BSLOPE(JE,L).GT.cg%TANPHI) THEN
              JE = JE+1
              IF(JE.GE.cg%JMAX(L)) GOTO 103
              GOTO 102
            ENDIF
          ENDIF
 103      JD = JE-cg%JR
          IF(JD.GE.2) THEN
            DO 104 J=JR1,JE-1
              DUM=DBLE(JE-J)/DBLE(JD)
              QRAW(J)=DUM*QRAW(cg%JR)
 104        CONTINUE
          ENDIF
C     Subr.15 EXTRAPO, extrapolates for nodes from J1 to J2
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QBX)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QSX)
          CALL EXTRAPO(cg,JE, JMAXL, QRAW)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%PB)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%PS)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%VS)
          GOTO 900
        ENDIF
      ENDIF
C     End of cg%IANGLE=0 in wet zone ..........................................
C
C     ....... Wet and Dry Zone for cg%IANGLE=0 and 1 ..........................
C     For node J=cg%JWD to cg%JDRY in wet and dry (WD) zone
C     PBWD(J)=bedload probability computed in Subr.18 PROBWD
C     PSWD(J)=suspended load probability computed in Subr.18 PROBWD
C     VSWD(J)=suspended sediment volume per unit area(m)
C     QSXWD(J)=cross-shore suspended sediment transport rate(m*m/s)
C     QBXWD(J)=cross-shore bedload sediment transport rate(m*m/s)
C     where hydrodynamic variables in WD zone are computed in Subr.16 WETDRY
C     HDIP(J)=mean water depth adjusted for dip in wet
C     and dry zone used for suspended sediment transport rate
C     if cg%IVWALL(L)=0 (no vertical wall at landward end)
C
 999  CONTINUE
      IF(cg%IOVER.EQ.1.AND.cg%JDRY.GT.cg%JR) THEN
        IF(cg%IVWALL(L).EQ.0) THEN
          J=cg%JWD
          HDIP(J)=cg%H(J)
          ZBPEAK=cg%ZB(J,L)
 140      J=J+1
          IF(J.EQ.cg%JDRY) GOTO 142
          IF(J.GT.cg%JDRY) GOTO 145
          IF(cg%ZB(J-1,L).LT.cg%ZB(J,L).AND.cg%ZB(J,L).GE.cg%ZB(J+1,L))
     +       ZBPEAK=cg%ZB(J,L)
          DUM=ZBPEAK-cg%ZB(J,L)
          IF(DUM.GT.cg%H(J)) THEN
            HDIP(J)=DUM
          ELSE
            HDIP(J)=cg%H(J)
          ENDIF
          IF(J.LT.cg%JCREST(L)) GOTO 140
 142      J=cg%JDRY
          HDIP(J)=cg%H(J)
          ZBPEAK=cg%ZB(J,L)
 141      J=J-1
          IF(J.LE.cg%JCREST(L)) GOTO 145
          IF(cg%ZB(J-1,L).LT.cg%ZB(J,L).AND.cg%ZB(J,L).GE.cg%ZB(J+1,L))
     +       ZBPEAK=cg%ZB(J,L)
          DUM=ZBPEAK-cg%ZB(J,L)
          IF(DUM.GT.cg%H(J)) THEN
            HDIP(J)=DUM
          ELSE
            HDIP(J)=cg%H(J)
          ENDIF
          GOTO 141
        ENDIF
 145    CONTINUE
C     For gravel tests MH and MB (IGMILD=1), landward extension
C     of bedload was necessary
        IF(IGMILD.EQ.1) JEXT=cg%JWD+NINT(4.2D0*cg%HRMS(1)/cg%DX)
C
        DO 150 J=cg%JWD,cg%JDRY
          IF(cg%IPERM.EQ.0.AND.cg%INFILT.EQ.0) THEN
            cg%QWX=cg%QO(L)
            IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
              IF(J.GE.cg%JX2) cg%QWX=cg%QM
              IF(J.GT.cg%JXW.AND.J.LT.cg%JX2) THEN
                cg%QWX=cg%QO(L)-(cg%QO(L)-cg%QM)*(cg%XB(J)-
     +          cg%XB(cg%JXW))/(cg%XB(cg%JX2)-cg%XB(cg%JXW))
              ENDIF
            ENDIF
          ELSE
            cg%QWX=cg%QO(L)-cg%QP(J)
          ENDIF
          cg%USWD(J)=cg%QWX/cg%H(J)-cg%AQWD*
     +          DSQRT(cg%GRAV*cg%H(J)/cg%PWET(J))
          IF(cg%D50.LT.cg%CSEDIA) THEN
            UCB=DSQRT(cg%GSD50S/cg%FB2(J,L))
          ELSE
            UCB=cg%GSD50S
          ENDIF
          PWAGH=cg%PWET(J)/cg%AGWD/cg%GRAV/cg%H(J)
          CALL PROBWD(cg%PWET(J),PWAGH,cg%USWD(J),UCB,PBWD(J))
          UCS=cg%WF/cg%FB2(J,L)**0.333333D0
          CALL PROBWD(cg%PWET(J),PWAGH,cg%USWD(J),UCS,PSWD(J))
          IF(PSWD(J).GT.PBWD(J)) PSWD(J)=PBWD(J)
C
C     Suspended load VBF and bedload factor BLDS in wet and dry zone
C     are adjusted so that cg%VS(J)=VSWD(J) and cg%QBX(J)=QBXWD(J) at J=cg%JWD
          IF(J.EQ.cg%JWD) THEN
            VBF=1.D0
            BLDS=1.D0
          ENDIF
          VSWD(J)=VBF*PSWD(J)
          VSWD(J)=VSWD(J)*DSQRT(1.D0+cg%BSLOPE(J,L)*cg%BSLOPE(J,L))
          QBXWD(J)=BLDS*PBWD(J)*cg%GSLOPE(J)*cg%USTD(J)**3.D0
          IF(J.EQ.cg%JWD) THEN
            IF(VSWD(J).GT.1.D-20)THEN
              VBF=cg%VS(J)/VSWD(J)
            ELSE
              VBF=0.D0
            ENDIF
            VSWD(J)=cg%VS(J)
            IF(DABS(QBXWD(J)).GT.1.D-20)THEN
              BLDS=cg%QBX(J)/QBXWD(J)
            ELSE
              BLDS=cg%BLD
            ENDIF
            QBXWD(J)=cg%QBX(J)
          ENDIF
          IF(cg%ISEDAV.GE.1.OR.cg%ISTSAN.EQ.1) THEN
            IF(cg%ISEDAV.GE.1) THEN
              DUM=cg%HP(J,L)
              IF(cg%ISEDAV.EQ.2) THEN
                  DUM=cg%ZB(J,L)-cg%ZMESH(J,L)
                  IF(DUM.LT.0.D0) DUM=0.D0
              ENDIF
              IF(DUM.GE.cg%D50) THEN
                  BRF=1.D0
              ELSE
                  BRF=(DUM/cg%D50)**cg%BEDLM
              ENDIF
            ELSE
              BRF=DEXP(-cg%CPSTON*cg%HP(J,L)/cg%D50)
            ENDIF
            IF(cg%IVWALL(L).EQ.0) VSWD(J)=BRF*VSWD(J)
            QBXWD(J)=BRF*QBXWD(J)
          ENDIF
          QSXWD(J)=ASLOPE(J)*cg%UMEAN(J)*VSWD(J)
          IF(cg%IOVER.EQ.1) THEN
            DUM = cg%H(J)
            IF(cg%IVWALL(L).EQ.0) DUM=HDIP(J)
            IF(DUM.LT.cg%HWDMIN) DUM = cg%HWDMIN
            AO=cg%SLPOT
            DUMQ=cg%QO(L)
            IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
              IF(J.GE.cg%JCREST(L).AND.J.LT.cg%JX2) DUMQ=cg%QD
              IF(J.GE.cg%JX2) DUMQ=cg%QM
            ENDIF
            QSXWD(J)=QSXWD(J)+AO*VSWD(J)*DUMQ/DUM
          ENDIF
C
C     If IGMILD=1, adjust QBXWD as follows
            IF(IGMILD.EQ.1) THEN
              IF(J.LE.JEXT) QBXWD(J)=QBXWD(cg%JWD)
              IF(J.EQ.cg%JWD) THEN
                JWD1=cg%JWD+1
                IF(JWD1.LT.cg%JR) THEN
                  DO 149 JJ=JWD1,cg%JR
                    cg%QBX(JJ)=cg%QBX(cg%JWD)
 149              CONTINUE
                ENDIF
              ENDIF
            ENDIF
            QRAWD(J)=(QBXWD(J)+QSXWD(J))/cg%SPORO1
            IF(cg%IANGLE.EQ.1) THEN
              US=cg%UMEAN(J)/cg%USTD(J)
              DUM=(1.D0+US*US)*cg%VMEAN(J)/cg%USTD(J)+
     +          2.D0*US*cg%STHETA(J)
              QBYWD(J)=QBXWD(J)*DUM/cg%GSLOPE(J)
              QSYWD(J)=cg%VMEAN(J)*VSWD(J)
            ENDIF
 150      CONTINUE
C
C     If cg%IPOND=1 and cg%NOPOND=0, ponded water exists between
C     nodes J=cg%JXW and cg%JX2. Ponded water is assumed to cause
C     sedimentation where cg%WF=sediment fall velocity and
C     cg%QD=wave-induced onshore volume flux at ridge crest
C     node cg%JCREST for deposition
          IF(cg%IPOND.EQ.1.AND.cg%NOPOND.EQ.0) THEN
            JDUM=cg%JDRY
            DLEN=(cg%XB(cg%JX2)-cg%XB(cg%JXW))/cg%SLPOT
C           DUM=cg%QD/cg%WF
C           IF(DLEN.LT.DUM) DLEN=DUM
            IF(JDUM.GT.cg%JXW) THEN
              JXW1=cg%JXW+1
              DO 151 J=JXW1, JDUM
                DUM=DEXP(-(cg%XB(J)-cg%XB(cg%JXW))/DLEN)
                PBWD(J)=PBWD(J)*DUM
                VSWD(J)=VSWD(J)*DUM
                PSWD(J)=PSWD(J)*DUM
                QBXWD(J)=QBXWD(J)*DUM
                QSXWD(J)=QSXWD(J)*DUM
                QRAWD(J)=(QBXWD(J)+QSXWD(J))/cg%SPORO1
                IF(cg%IANGLE.EQ.1) THEN
                  QBYWD(J)=QBYWD(J)*DUM
                  QSYWD(J)=QSYWD(J)*DUM
                ENDIF
 151          CONTINUE
            ENDIF
          ENDIF
C
C     Connect wet variables (J=1 to cg%JR) with WD variables
C     (J=cg%JWD to cg%JDRY) using Subr.17 TRANWD
          IF(cg%JDRY.GT.cg%JR) THEN
            CALL TRANWD(cg%PB,cg%JR,PBWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%PS,cg%JR,PSWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%VS,cg%JR,VSWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%QSX,cg%JR,QSXWD,cg%JWD,cg%JDRY)
            CALL TRANWD(cg%QBX,cg%JR,QBXWD,cg%JWD,cg%JDRY)
            CALL TRANWD(QRAW,cg%JR,QRAWD,cg%JWD,cg%JDRY)
            IF(cg%IANGLE.EQ.1) THEN
              CALL TRANWD(cg%QSY,cg%JR,QSYWD,cg%JWD,cg%JDRY)
              CALL TRANWD(cg%QBY,cg%JR,QBYWD,cg%JWD,cg%JDRY)
            ENDIF
          ENDIF
C
C     Compute sediment transport in landward wet zone of wave transmission
C     Suspended load VBF and bedload factor BQ are adjusted so that
C     cg%VS(cg%JSL)=cg%VS(cg%JSL1) and cg%QBX(cg%JSL)=cg%QBX(cg%JSL1)
        IF(cg%IWTRAN.EQ.1.AND.cg%JDRY.EQ.cg%JSL1) THEN
        BQ=1.D0
        VBF=1.D0
        IF(cg%JSL.GE.cg%JMAX(L)) GOTO 165
        DO 160 J=cg%JSL,cg%JMAX(L)
          IF(cg%D50.LT.cg%CSEDIA) THEN
            RB(J) = DSQRT(cg%GSD50S/cg%FB2(J,L))/cg%USTD(J)
          ELSE
            RB(J)=cg%GSD50S/cg%USTD(J)
          ENDIF
          RS(J) = cg%WF/cg%USTD(J)/cg%FB2(J,L)**0.3333D0
          US = cg%UMEAN(J)/cg%USTD(J)
          cg%PB(J)=0.5D0*(ERFCC((RB(J)+US)/cg%SQR2 )+
     +          ERFCC((RB(J)-US)/cg%SQR2))
          cg%PS(J)=0.5D0*(ERFCC((RS(J)+US)/cg%SQR2 )+
     +          ERFCC((RS(J)-US)/cg%SQR2))
          IF(cg%PS(J).GT.cg%PB(J)) cg%PS(J) = cg%PB(J)
          cg%VS(J) = cg%PS(J)*VBF
          cg%VS(J) = cg%VS(J)*DSQRT(1.D0+cg%BSLOPE(J,L)*cg%BSLOPE(J,L))
          cg%QBX(J) = BQ*cg%PB(J)*cg%GSLOPE(J)*cg%USTD(J)**3
          IF(J.EQ.cg%JSL) THEN
            IF(cg%VS(J).GT.1.D-20)THEN
              VBF=cg%VS(cg%JSL1)/cg%VS(J)
            ELSE
              VBF=0.D0
            ENDIF
            cg%VS(J)=cg%VS(cg%JSL1)
            IF(DABS(cg%QBX(J)).GT.1.D-20)THEN
              BQ=cg%QBX(cg%JSL1)/cg%QBX(J)
            ELSE
              BQ=cg%BLD
            ENDIF
            cg%QBX(J)=cg%QBX(cg%JSL1)
          ENDIF
          IF(cg%ISEDAV.GE.1.OR.cg%ISTSAN.EQ.1)THEN
            IF(cg%ISEDAV.GE.1) THEN
              DUM=cg%HP(J,L)
              IF(cg%ISEDAV.EQ.2) THEN
                  DUM=cg%ZB(J,L)-cg%ZMESH(J,L)
                  IF(DUM.LT.0.D0) DUM=0.D0
              ENDIF
              IF(DUM.GE.cg%D50)THEN
                  BRF=1.D0
              ELSE
                  BRF=(DUM/cg%D50)**cg%BEDLM
              ENDIF
            ELSE
              BRF=DEXP(-cg%CPSTON*cg%HP(J,L)/cg%D50)
            ENDIF
            cg%VS(J)=BRF*cg%VS(J)
            cg%QBX(J)=BRF*cg%QBX(J)
          ENDIF
          cg%QSX(J) = ASLOPE(J)*cg%UMEAN(J)*cg%VS(J)
          QRAW(J) = (cg%QBX(J) + cg%QSX(J))/cg%SPORO1
          IF(cg%IANGLE.EQ.1) THEN
            cg%QBY(J)=0.D0
            cg%QSY(J)=0.D0
          ENDIF
 160    CONTINUE
 165    CONTINUE
        ELSE
C     Connect cg%QSX(J), cg%QBX(J) and QRAW(J)=0.0 landward of cg%JDRY for no wave transmission
C     (cg%IWTRAN=0) or no wave overtopping to landward wet zone even if cg%IWTRAN=1
          IF(cg%JDRY.LT.cg%JMAX(L)) THEN
            JDRY1=cg%JDRY+1
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%QSX)
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%QBX)
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),QRAW)
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%PB)
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%PS)
            CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%VS)
            IF(cg%IANGLE.EQ.1) THEN
              CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%QSY)
              CALL EXTRAPO(cg,JDRY1,cg%JMAX(L),cg%QBY)
            ENDIF
          ENDIF
        ENDIF
C
        GOTO 900
C
      ENDIF
C     End of Wet and Dry Zone for cg%IANGLE=0 and 1 .......................
C
C.....Obliquely Incident Waves in wet zone .............................
 888  CONTINUE
      IF(cg%IANGLE.EQ.1) THEN
        DO 190 J=1,cg%JR
          SIGT = cg%USTD(J)/cg%CTHETA(J)
          IF(cg%D50.LT.cg%CSEDIA) THEN
            RB(J)= DSQRT(cg%GSD50S/cg%FB2(J,L))/SIGT
          ELSE
            RB(J)=cg%GSD50S/SIGT
          ENDIF
          RS(J)= cg%WF/SIGT/cg%FB2(J,L)**0.3333D0
          WSTA = cg%USTA(J)*cg%CTHETA(J) + cg%VSTA(J)*cg%STHETA(J)
          VCUS = cg%VSTA(J)*cg%CTHETA(J) -cg%USTA(J)*cg%STHETA(J)
          FS = RS(J)*RS(J) - VCUS*VCUS
          IF(FS.LT.0.D0) THEN
            cg%PS(J) = 1.D0
          ELSE
            FS = DSQRT(FS)
            cg%PS(J)= 0.5D0*(ERFCC((FS+WSTA)/cg%SQR2)+
     +          ERFCC((FS-WSTA)/cg%SQR2))
          ENDIF
          FB = RB(J)*RB(J) - VCUS*VCUS
          IF(FB.LT.0.D0) THEN
            cg%PB(J) = 1.D0
          ELSE
            FB = DSQRT(FB)
            cg%PB(J)= 0.5D0*(ERFCC((FB+WSTA)/cg%SQR2)+
     +          ERFCC((FB-WSTA)/cg%SQR2))
          ENDIF
          IF(cg%PS(J).GT.cg%PB(J)) cg%PS(J)=cg%PB(J)
          IF(cg%IROLL.EQ.0) THEN
            cg%VS(J) = cg%PS(J)*(cg%EFFF*cg%DFSTA(J)+
     +          cg%EFFB*cg%DBSTA(J))/cg%WFSGM1
          ELSE
            cg%VS(J) = cg%PS(J)*(cg%EFFF*cg%DFSTA(J)+
     +          cg%EFFB*cg%RBETA(J)*cg%RQ(J))/cg%WFSGM1
          ENDIF
          cg%VS(J) = cg%VS(J)*DSQRT(1.D0+cg%BSLOPE(J,L)*cg%BSLOPE(J,L))
          VSTA2 = cg%VSTA(J)*cg%VSTA(J)
          TWOS = 2.D0*cg%STHETA(J)
          BQ=cg%BLD
C     Input bedload parameter in Subr.2 INPUT is adjusted to account
C     for cg%QBREAK=fraction(0.0 to 1.0) of breaking waves.
C     Put "C" in front of the next line for no adjustment
          IF(cg%D50.LT.cg%CSEDIA)BQ=BQ*(0.5D0+cg%QBREAK(J))
          IF(IGMILD.EQ.1) THEN
            BQ=cg%BLD*(1.D0+BQCOEFF*cg%QBREAK(J))
          ENDIF
C     BDJ added 2012-10-23
            DECAYL = MIN(cg%XB(cg%JSWL(L))/4.D0,2.D0*cg%TP*cg%CP(1)) ! The decay length
            JDECAY = NINT(DECAYL/cg%DX)! index of decay intrusion length
C     end BDJ added 2012-10-23
          DUM = BQ*cg%PB(J)*(cg%USTD(J)*cg%USTD(J) +
     +          cg%VSTD(J)*cg%VSTD(J))**1.5D0
          cg%QBX(J) = DUM*cg%GSLOPE(J)*(1.D0 +
     +          cg%USTA(J)*VSTA2 + TWOS*VCUS)
          cg%QBY(J) = DUM*(cg%VSTA(J)*(1.D0 +
     +          cg%USTA(J)*cg%USTA(J)+ VSTA2)+
     +       TWOS*WSTA)
          IF(cg%ISEDAV.GE.1.OR.cg%ISTSAN.EQ.1) THEN
            IF(cg%ISEDAV.GE.1) THEN
              DUM=cg%HP(J,L)
              IF(cg%ISEDAV.EQ.2) THEN
                   DUM=cg%ZB(J,L)-cg%ZMESH(J,L)
                   IF(DUM.LT.0.D0) DUM=0.D0
              ENDIF
              IF(DUM.GE.cg%D50) THEN
                   BRF=1.D0
              ELSE
                   BRF=(DUM/cg%D50)**cg%BEDLM
              ENDIF
            ELSE
              BRF=DEXP(-cg%CPSTON*cg%HP(J,L)/cg%D50)
            ENDIF
            cg%VS(J)=BRF*cg%VS(J)
            cg%QBX(J)=BRF*cg%QBX(J)
            cg%QBY(J)=BRF*cg%QBY(J)
          ENDIF
          cg%QSX(J) = ASLOPE(J)*cg%UMEAN(J)*cg%VS(J)
          IF(cg%IOVER.EQ.1) THEN
            DUM = cg%H(J)
            IF(DUM.LT.cg%HWDMIN) DUM = cg%HWDMIN
            AO=cg%SLPOT
            DUMQ=cg%QO(L)
            cg%QSX(J)=cg%QSX(J)+AO*cg%VS(J)*DUMQ/DUM
          ENDIF
          cg%QSY(J) = cg%VMEAN(J)*cg%VS(J)
          QRAW(J) = (cg%QBX(J) + cg%QSX(J))/cg%SPORO1
 190      CONTINUE
C
C     BDJ added on 2012-10-24
        cg%QSX(1:JDECAY) = cg%QSX(JDECAY)
        cg%QBX(1:JDECAY) = cg%QBX(JDECAY)
        QRAW(1:JDECAY) = QRAW(JDECAY)
C     end BDJ added on 2012-10-24
C     Scarping extrapolation is included for oblique waves as well
        IF(cg%IOVER.EQ.0.OR.cg%JDRY.LE.cg%JR) THEN
          JR1 = cg%JR+1
          JE = JR1
          IF(QRAW(cg%JR).LT.0.D0) THEN
 202        IF(cg%BSLOPE(JE,L).GT.cg%TANPHI) THEN
              JE = JE+1
              IF(JE.GE.cg%JMAX(L)) GOTO 203
              GOTO 202
            ENDIF
          ENDIF
 203      JD = JE-cg%JR
          IF(JD.GE.2) THEN
            DO 204 J=JR1, JE-1
              DUM=DBLE(JE-J)/DBLE(JD)
              QRAW(J) =DUM*QRAW(cg%JR)
 204        CONTINUE
          ENDIF
C
C     Subr. 15 EXTRAPO extrapolates for nodes from J1 to J2
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QBX)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QSX)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QBY)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%QSY)
          CALL EXTRAPO(cg,JE, JMAXL, QRAW)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%PB)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%PS)
          CALL EXTRAPO(cg,JR1, JMAXL, cg%VS)
          GOTO 900
        ELSE
          GOTO 999
        ENDIF
C
      ENDIF
C     End of cg%IANGLE=1 in wet zone ..........................................
C
C     Adjust computed cg%QSX(1) and cg%QBX(1) at node 1 to be consistent with
C     the boundary condition used in Subr.12 CHANGE
  900 CONTINUE
      cg%QSX(1)=cg%QSX(2)
      cg%QBX(1)=cg%QBX(2)
      QRAW(1)=QRAW(2)
      IF(cg%IANGLE.EQ.1)THEN
        cg%QSY(1)=cg%QSY(2)
        cg%QBY(1)=cg%QBY(2)
      ENDIF
C     Adjust sediment transport rates at node cg%JMAX to be consitent with the
C     boundary condition used in subr.12 CHANGE
      JMAX1=cg%JMAX(L)-1
      cg%QSX(JMAXL)=cg%QSX(JMAX1)
      cg%QBX(JMAXL)=cg%QBX(JMAX1)
      QRAW(JMAXL)=QRAW(JMAX1)
C     Smoothing QRAW (before cg%DELZB is computed) using Sub.14 SMOOTH
      CALL SMOOTH(cg,JMAXL,QRAW,cg%Q)
C
      RETURN
      END SUBROUTINE SEDTRA
C
C     --11-----------------  END OF SUBROUTINE SEDTRA  ---------------------


C     #23##################### SUBROUTINE SRFSP ##############################
C
C     This subroutine computes the surf similarity parameter
      SUBROUTINE SRFSP(cg,L)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER(NN=5000, NL=100)
C
      CO = cg%GRAV*cg%TP/cg%TWOPI
      ARG = ABS(CO/cg%CP(1)*cg%STHETA(1))
      ARG = MIN(ARG,cg%SQR2/2.D0) ! Arbitrary max deep water cg%ANGLE of 45 deg
      THETAO=DASIN(ARG)
      HRMSO = cg%HRMS(1)*DSQRT((cg%CP(1)*cg%WN(1)*cg%CTHETA(1))/
     +     (0.5D0*CO*DCOS(THETAO)))

C First guess at slope uses SWS slope
      TANB = (cg%ZB(cg%JR+1,L)-cg%ZB(cg%JR-1,L))/
     +          (cg%XB(cg%JR+1)-cg%XB(cg%JR-1))
      cg%SSP = TANB/DSQRT(cg%SQR2*HRMSO/(cg%TWOPI/cg%WKPO))
C Just to improve slope estimate, estimate Runup with Mase 1989:
C      R2P = cg%SQR2*HRMSO*1.86D0*cg%SSP**0.71D0
      RETURN
      END SUBROUTINE SRFSP
C
C     -23------------------------- END OF SUBROUTINE SRFSP -----------------------


C     --------------------------SUBROUTINE VSTGBY-------------------------
C     This subroutine computes VSIGT= VMEAN/SIGT for specified GBY,
C     CTHETA, USIGT=UMEAN/SIGT, and STHETA but neglects USIGT*STHETA
C
      SUBROUTINE VSTGBY(cg,CTHETA,USIGT,STHETA,VSIGT,GBY)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
C
      DUM=USIGT*STHETA
C     which is assumed to be zero
C
      VSIGT = 0.D0
      IF(GBY.EQ.0.D0) GOTO 100
      B = cg%SQRG1*(1.D0+STHETA*STHETA)
      C = GBY
      IF(GBY.GT.0.D0) THEN
        D=B*B + 4.D0*CTHETA*C
        IF(D.GE.0.D0) VSIGT=0.5D0*(DSQRT(D)-B)/CTHETA
        IF(VSIGT.LT.0.D0) VSIGT=0.D0
C
      ELSE
        D = B*B-4.0D0*CTHETA*C
        IF(D.GE.0.D0) VSIGT=0.5D0*(B-DSQRT(D))/CTHETA
        IF(VSIGT.GT.0.D0) VSIGT=0.D0
      ENDIF
C
 100    CONTINUE
      RETURN
      END SUBROUTINE VSTGBY
C
C     -------------------END OF SUBROUTINE VSTGBY-------------------------


C     #21##################### SUBROUTINE WTRANS ##############################
C
C     This subroutine computes transmitted waves (IWTRAN=1) landward of
C     structure along cross-shore line L if landward standing water exists
C
      SUBROUTINE WTRANS(cg,ITIME,L)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NB=30000,NL=100)
C
      DATA CBREAK/10.0D0/
C
C     cg%JDRY=landward end node of wet and dry zone computation
C     cg%JSL=most seaward wet node in landward wet zone
C     cg%JSL1=(cg%JSL-1)=most landward node in wet and dry zone
C     If cg%JDRY is less than cg%JSL1, no wave overtopping occurs but
C     wave transmission through a porous structure is included
C
C     In landward wet zone, volume flux is constant where computed
C     wave overtopping rate cg%QO(L) along cross-shore line L includes
C     water flux cg%QP in porous layer whose vertical thickness is cg%HP
      IF(cg%IPERM.EQ.1) QPHP=cg%QP(cg%JSL1)/cg%HP(cg%JSL1,L)
C     which is assumed to be constant in landward wet zone
C
      ICHECK=0
      DUM=CBREAK*cg%H(cg%JSL1)
      IF(DUM.LT.0.01D0) DUM=0.01D0
      DO 100 J=cg%JSL, cg%JMAX(L)
        cg%PWET(J)=1.D0
        IF(ICHECK.EQ.0) THEN
          IF(cg%SWLDEP(J,L).GT.DUM) THEN
            JLIN=J
            ICHECK=1
          ENDIF
        ENDIF
        IF(cg%IPERM.EQ.1) THEN
          cg%UPMEAN(J)=QPHP
          IF(cg%HP(J,L).LE.0.D0) cg%UPMEAN(J)=0.D0
          cg%QP(J)=cg%UPMEAN(J)*cg%HP(J,L)
        ENDIF
        IF(cg%IANGLE.EQ.1) THEN
          cg%STHETA(J)=0.D0
          cg%VMEAN(J)=0.D0
          cg%VSTD(J)=0.D0
        ENDIF
  100   CONTINUE
        IF(ICHECK.EQ.0)JLIN=cg%JMAX(L)
C
        cg%WSETUP(cg%JSL1)=cg%H(cg%JSL1)+cg%ZB(cg%JSL1,L)-
     +          cg%SWLAND(ITIME)
        CSIGMA=cg%SIGMA(cg%JSL1)
        JDUM=cg%JSWL(L)
        IF(CSIGMA.GT.cg%SIGMA(JDUM)) CSIGMA=cg%SIGMA(JDUM)
        IF(CSIGMA.GT.cg%SIGMA(1)) CSIGMA=cg%SIGMA(1)
        DO 110 J=JLIN,cg%JMAX(L)
        cg%H(J)=cg%SWLDEP(J,L)+cg%WSETUP(cg%JSL1)
C       cg%H(J)=cg%SWLDEP(J,L)
        cg%WSETUP(J)=cg%WSETUP(cg%JSL1)
C       cg%WSETUP(J)=0.D0
        cg%SIGMA(J)=CSIGMA
        IF(cg%H(J).LT.1.D-3) cg%H(J)=1.D-3
        cg%CP(J)=DSQRT(cg%GRAV*cg%H(J))
        cg%QWX=cg%QO(L)
        IF(cg%IPERM.EQ.1)cg%QWX=cg%QWX-cg%QP(J)
        IF(cg%SIGMA(J).LE.0.D0) THEN
          IF(cg%QWX.LE.0.D0) THEN
            cg%SIGMA(J)=0.D0
          ELSE
            cg%SIGMA(J)=DSQRT(cg%QWX*cg%H(J)/cg%CP(J))
            IF(cg%SIGMA(J).GT.cg%SIGMA(cg%JSWL(L)))
     +          cg%SIGMA(J)=cg%SIGMA(cg%JSWL(L))
            IF(cg%SIGMA(J).GT.cg%SIGMA(1)) cg%SIGMA(J)=cg%SIGMA(1)
          ENDIF
        ENDIF
        cg%SIGSTA(J)=cg%SIGMA(J)/cg%H(J)
        cg%UMEAN(J)=cg%QWX/cg%H(J)-cg%CP(J)*cg%SIGSTA(J)*cg%SIGSTA(J)
C       cg%UMEAN(J)=0.D0
        cg%USTD(J)=cg%CP(J)*cg%SIGSTA(J)
  110   CONTINUE
C
C     Linear interpolation for transition zone from node cg%JSL1
C     where wet probability is less than unity to wet node JLIN
C     Assume cg%WSETUP(J) above SWL and cg%SIGMA(J) remain constant
C     for nodes J=cg%JSL1,...,cg%JMAX(J) for wave transmission
      DUM=DBLE(JLIN-cg%JSL1)
      IF(DUM.LE.1.D0) GOTO 999
      DO 120 J=cg%JSL,JLIN-1
        DJ=DBLE(J-cg%JSL1)/DUM
        DJ1=1.D0-DJ
        cg%WSETUP(J)=DJ1*cg%WSETUP(cg%JSL1)+DJ*cg%WSETUP(JLIN)
        cg%H(J)=cg%WSETUP(J)+cg%SWLDEP(J,L)
        cg%SIGMA(J)=DJ1*cg%SIGMA(cg%JSL1)+DJ*cg%SIGMA(JLIN)
        cg%UMEAN(J)=DJ1*cg%UMEAN(cg%JSL1)+DJ*cg%UMEAN(JLIN)
        cg%USTD(J)=DJ1*cg%USTD(cg%JSL1)+DJ*cg%USTD(JLIN)
  120 CONTINUE
C
  999 CONTINUE
      RETURN
      END SUBROUTINE WTRANS
C
C     -21------------------------- END OF SUBROUTINE WTRANS -----------------------


c #28######################## SUBROUTINE PHASEAVEFV #############################
c This subroutine calculates the phase-averaged drag force Fv
c   For submerged vegetation, the parametric model is used
c   For emergent vegetation, two models are used:
c     (1) parametric model with hv/h = 0.9 and different sets of Cd
c     (2) hybrid model with Fv = Fv(SF, hv/h = 0.55) + Fv(LWT)

      SUBROUTINE PHASEAVEFV(cg,J,L,WHRMS,D,STREAMSTRESSSTA, FVCWLWT)
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      type (cshore_vars_type), intent (inout) :: cg
      PARAMETER (NN=5000,NL=100,NFR=500,NNZ=50)
C     NFR=maximum number of frequency beams for JONSWAP spectrum

      DOUBLE PRECISION URSELL, H2H, HV2H, MMOD, HV2HTOMEME
c cg%IFV = 1: original code
c cg%IFV = 2: parametric model of Fv
c cg%IFV = 3: hybrid model of Fv

      IF (cg%IFV.EQ.1) THEN
        STREAMSTRESSSTA = cg%FSX*(cg%DFSTA(J)+cg%DVEGSTA(J))/
     +          (cg%WN(J)*cg%CP(J))
      ELSEIF (cg%IFV.EQ.2) THEN
        STREAMSTRESSSTA = 0.0D0
        IF (cg%VEGN(J,L).GT.0.0D0) THEN
           TMEAN = cg%WT(J)
c                       /1.35D0
           CALL DISPERSION(D, TMEAN , WKMEAN)
           OMEGAMEAN = 2.0D0*cg%PI/TMEAN
           WVLENMEAN = 2.0D0*cg%PI/WKMEAN
           URSELL= WHRMS*DSQRT(2.0D0) * WVLENMEAN**2.0D0/D**3.0D0
           HS2H  = WHRMS*DSQRT(2.0D0) / D
           HV2H  = cg%VEGD(J,L)/D

           CALL FINDHV2HTOMEME(MIN(URSELL,1000.0D0),HS2H,HV2H,HV2HTOM)

           STREAMSTRESSSTA = 0.5D0*cg%VEGCD*cg%VEGB(J,L)*cg%VEGN(J,L)*
     +              OMEGAMEAN**2.0D0*WHRMS**3.0D0*
     +              DCOSH(WKMEAN*D*MIN(HV2H, 1.0D0))**2.0D0/
     +              DSINH(WKMEAN*D)**2.0D0*
     +              HV2HTOM/DSQRT(cg%PI)/8.0D0/cg%GRAV
        ENDIF
      ELSEIF (cg%IFV.EQ.3) THEN
        STREAMSTRESSSTA = 0.0D0
        IF (cg%VEGN(J,L).GT.0.0D0) THEN
          IF(cg%VEGD(J,L).LT.D) THEN
            TMEAN = cg%WT(J)
c/1.35D0
            CALL DISPERSION(D, TMEAN , WKMEAN)
            OMEGAMEAN = 2.0D0*cg%PI/TMEAN
            WVLENMEAN = 2.0D0*cg%PI/WKMEAN
            URSELL= WHRMS*DSQRT(2.0D0) * WVLENMEAN**2.0D0/D**3.0D0
            HS2H  = WHRMS*DSQRT(2.0D0) / D
            HV2H  = cg%VEGD(J,L)/D
            CALL FINDHV2HTOMEME(MIN(URSELL,1000.0D0),HS2H,HV2H,HV2HTOM)

            STREAMSTRESSSTA = 0.5D0*cg%VEGCD*cg%VEGB(J,L)*cg%VEGN(J,L)*
     +                   OMEGAMEAN**2.0D0*WHRMS**3.0D0*
     +                   DCOSH(WKMEAN*D*MIN(HV2H, 1.0D0))**2.0D0/
     +                   DSINH(WKMEAN*D)**2.0D0*
     +                   HV2HTOM/DSQRT(cg%PI)/8.0D0/cg%GRAV
         ELSE
            TMEAN = cg%WT(J)
c/1.35D0
            CALL DISPERSION(D, TMEAN , WKMEAN)
            OMEGAMEAN = 2.0D0*cg%PI/TMEAN
            WVLENMEAN = 2.0D0*cg%PI/WKMEAN
            URSELL= WHRMS*DSQRT(2.0D0) * WVLENMEAN**2.0D0/D**3.0D0
            HS2H  = WHRMS*DSQRT(2.0D0) / D
            HV2H = 0.55D0
            CALL FINDHV2HTOMEME(MIN(URSELL,1000.0D0),HS2H,HV2H,HV2HTOM)

            STREAMSTRESSSTA = 0.5D0*cg%VEGCD*cg%VEGB(J,L)*cg%VEGN(J,L)*
     +                     OMEGAMEAN**2.0D0*WHRMS**3.0D0*
     +                     DCOSH(WKMEAN*D*MIN(HV2H, 1.0D0))**2.0D0/
     +                     DSINH(WKMEAN*D)**2.0D0*
     +                     HV2HTOM/DSQRT(cg%PI)/8.0D0/cg%GRAV
C For emergent vegetation, Fv_LWT is from Dean and Bener (2006) Eq. 12
            STREAMSTRSTALWT = cg%VEGCD*cg%VEGB(J,L)*cg%VEGN(J,L)*
     +                       (3.0D0*DSQRT(cg%PI)/4.0D0)*WHRMS**3.0D0
     +                       *cg%WKP/12.0D0/cg%PI/DTANH(cg%WKP*D)
            STREAMSTRESSSTA = STREAMSTRESSSTA + STREAMSTRSTALWT
          ENDIF
        ENDIF
      ENDIF

c Fv_cw from LWT
      FVCWLWT     = 0.5D0*cg%VEGCDM*cg%VEGB(J,L)*cg%VEGN(J,L)*
     +              2.0D0*(USIGT*SIGT)*OMEGA*0.5D0*
     +              (WHRMS*DSQRT(cg%PI)*0.5D0)*
     +              2.0D0/cg%PI*DSINH(cg%WKP*MIN(D, cg%VEGD(J,L)))/
     +              DSINH(cg%WKP*D) / cg%WKP /cg%GRAV

      RETURN
      END SUBROUTINE PHASEAVEFV

C     -28-----------------  END OF SUBROUTINE PHASEAVEFV  ---------------------



c     #26##################### SUBROUTINE FINDHV2HTOMEME ##############################

      SUBROUTINE FINDHV2HTOMEME(URSELL,H2H,HV2H,HV2HTOMEME)

      DOUBLE PRECISION URSELL, H2H, HV2H, MMOD, HV2HTOMEME

c     For emergent veg, use hv/h = 0.99 for now.
      HV2HMODIFIED = MIN(HV2H, 0.90D0);

c      IF (URSELL.LT.URALL(1)) THEN
c         MMOD=1.0D0+(MMODALL(1)-1)*URSELL/URALL(1)
c      ELSEIF (URSELL.GT.URALL(NINTERP)) THEN
c         MMOD=MMODALL(NINTERP)
c      ELSE
c         CALL INTERP1(NINTERP, URALL, MMODALL, URSELL, MMOD)
c      ENDIF
c
c      HV2HTOMEME1=(MMOD-1.0D0)*(0.75D0/DSINH(H2H*1.0D0)-1.0D0) + 1.0D0

CC Try Zhu and Chen (2018) JGR
      F1 = 3.58963D0 - 2.5823D0/(1.0D0+(URSELL/25.63303D0)**73.8627D0)
     +                        **(8.7748D0*0.0001D0)
CC  Note here that as a first try, hv/h is set as 0.95 for emergent vegetation
      IF (HV2HMODIFIED.LE.0.8D0) THEN
          F2 = 0.19242D0*(HV2HMODIFIED)**3.0D0 +
     +         0.07618D0*(HV2HMODIFIED)**2.0D0 +
     +         1.0054D0*(HV2HMODIFIED) + 0.64848D0
      ELSEIF (HV2HMODIFIED.GT.0.8D0.AND.HV2HMODIFIED.LT.1.0D0) THEN
          F2 = (29.8D0*HV2HMODIFIED**3.0D0-
     +      77.289D0*HV2HMODIFIED**2.0D0+68.249D0*HV2HMODIFIED
     +      -18.793D0)*MIN(1.0D0,
     +      1.0D0+(1.0D0/(2.0D0-DCOSH(HV2HMODIFIED-0.7D0)**2.8D0)-
     +       1.0D0)*
     +      ((EXP(H2H-0.2)-1.0D0) / (EXP(0.4D0) -1.0D0)))
      ENDIF

      A0 = 0.0D0
      A1 = 0.0D0
      A2 = 0.0D0
      A3 = 0.0D0
      A4 = 0.0D0
      A5 = 0.0D0
      MU = 0.0D0
      GAM = 1.0D0
      IF (URSELL.LE.1.0D0) THEN
         A0 = -1.2604E-3
         A1 = 7.0547E-3
         A2 = 1.9004E-3
         A3 = 0.0D0
         A4 = 0.0D0
         A5 = 0.0D0
         MU = 0.0D0
         GAM= 1.0D0
      ELSEIF (URSELL.GT.1.0D0.AND.URSELL.LE.11.0D0) THEN
         A0 = 2.7124E-3
         A1 = 6.1698E-4
         A2 = 4.2423E-3
         A3 = 7.6494E-4
         A4 = 6.3251E-5
         A5 = -1.9486E-6
         MU = 0.0D0
         GAM = 1.0D0
      ELSEIF (URSELL.GT.11.0D0.AND.URSELL.LE.50.0D0) THEN
         A0 = 3.1646E-1
         A1 = 7.30E-2
         A2 = -1.9153E-2
         A3 = 3.7518E-3
         A4 = 7.7545E-4
         A5 = -5.8145E-4
         MU = 30.75
         GAM= 11.33
      ELSEIF (URSELL.GT.50.0D0.AND.URSELL.LE.90.0D0) THEN
         A0 = 4.4339E-1
         A1 = 1.4262E-2
         A2 = -4.1255E-3
         A3 = 1.5321E-3
         A4 = 2.791E-5
         A5 = -2.1287E-4
         MU = 70.25
         GAM= 11.619
      ELSEIF (URSELL.GT.90.0D0.AND.URSELL.LE.112.0D0) THEN
         A0 = 4.6412E-1
         A1 = 6.4872E-4
         A2 = -2.469E-4
         A3 = 3.6116E-4
         A4 = -5.6746E-5
         A5 = -4.683E-5
         MU = 101.25
         GAM= 6.4226
      ELSEIF (URSELL.GT.112.0D0.AND.URSELL.LE.140.0D0) THEN
         A0 = 4.6412E-1
         A1 = -1.2267E-3
         A2 = -2.2371E-4
         A3 = 1.5531E-4
         A4 = -1.0501E-5
         A5 = -2.6202E-5
         MU = 126.25
         GAM= 8.1548
      ELSEIF (URSELL.GT.140.0D0.AND.URSELL.LE.200.0D0) THEN
         A0 = 4.5158E-1
         A1 = -6.6412E-3
         A2 = -1.1636E-4
         A3 = 3.9872E-4
         A4 = -6.0734E-5
         A5 = -1.0259E-4
         MU = 170.25
         GAM= 17.393
      ELSEIF (URSELL.GT.200.0D0.AND.URSELL.LE.400.0D0) THEN
         A0 = 3.9904E-1
         A1 = -2.1609E-2
         A2 = 1.5767E-3
         A3 = -3.8946E-4
         A4 = -1.4773E-4
         A5 = 1.3648E-4
         MU = 300.25
         GAM= 57.807
      ELSEIF (URSELL.GT.400.0D0.AND.URSELL.LE.600.0D0) THEN
         A0 = 3.3751E-1
         A1 = -1.4282E-2
         A2 = 9.9248E-4
         A3 = -2.4393E-4
         A4 = -5.5042E-5
         A5 = 5.732E-5
         MU = 500.25
         GAM= 57.807
      ELSEIF (URSELL.GT.600.0D0.AND.URSELL.LE.843.0D0) THEN
         A0 = 2.9175E-1
         A1 = -1.2498E-2
         A2 = 6.4769E-4
         A3 = 3.8943E-4
         A4 = 2.0522E-5
         A5 = -8.9579E-5
         MU = 721.75
         GAM= 70.22
      ELSEIF (URSELL.GT.843.0D0.AND.URSELL.LE.1000.0D0) THEN
         A0 = 2.6299E-1
         A1 = -6.1774E-3
         A2 = 3.8208E-4
         A3 = 3.6931E-4
         A4 = -1.5269E-5
         A5 = -6.543E-5
         MU = 921.75
         GAM= 45.394
      ENDIF

      URSELLHAT = (URSELL-MU)/GAM
      F3 = A0+A1*URSELLHAT+A2*URSELLHAT**2.0D0+
     +      A3*URSELLHAT**3.0D0+
     +      A4*URSELLHAT**4.0D0 + A5*URSELLHAT**5.0D0
      HV2HTOMEME=(F1/SINH(H2H)-1.0D0)*F3/
     +              (F2/SINH(HV2HMODIFIED)-1.0D0)
      END SUBROUTINE FINDHV2HTOMEME
c     #26##################### END OF SUBROUTINE FINDHV2HTOMEME ##############################

      function dump_vars(cg, lbl) result (s)
        implicit none
        type(cshore_vars_type), intent(in) :: cg
        character(len=*), intent(in) :: lbl
        integer :: s
        integer :: i, j, l

        OPEN(UNIT=99,FILE=trim(cg%basename)//'cshore_bmi_vars_'//
     +    trim(lbl),STATUS='UNKNOWN',ACCESS='SEQUENTIAL',IOSTAT=s)
        if (s /= 0) then
          s = 1
          return
        endif
        OPEN(UNIT=98,FILE=trim(cg%basename)//'cshore_bmi_vars_lookup',
     +    STATUS='UNKNOWN',ACCESS='SEQUENTIAL',IOSTAT=s)
        if (s /= 0) then
          s = 2
          return
        endif

        l = 0

        write(99,'(a)',IOSTAT=s) 'TIME'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TIME'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%TIME
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IPROFL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IPROFL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IPROFL
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IANGLE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IANGLE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IANGLE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IROLL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IROLL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IROLL
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IWIND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IWIND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IWIND
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IPERM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IPERM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IPERM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IOVER'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IOVER'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IOVER
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IWCINT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IWCINT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IWCINT
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ISEDAV'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ISEDAV'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ISEDAV
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IWTRAN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IWTRAN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IWTRAN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IVWALL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IVWALL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%IVWALL(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'ILAB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ILAB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ILAB
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'INFILT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',INFILT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%INFILT
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IPOND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IPOND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IPOND
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ITIDE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ITIDE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ITIDE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ILINE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ILINE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ILINE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IQYDY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IQYDY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IQYDY
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IVEG'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IVEG'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IVEG
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ICLAY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ICLAY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ICLAY
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ISMOOTH'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ISMOOTH'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ISMOOTH
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IDISS'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IDISS'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IDISS
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IFV'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IFV'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IFV
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IWEIBULL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IWEIBULL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IWEIBULL
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'TP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%TP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WKPO'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WKPO'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WKPO
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ANGLE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ANGLE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ANGLE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WT(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'FREQMIN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQMIN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FREQMIN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'FREQMAX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQMAX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FREQMAX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'FREQNUM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQNUM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FREQNUM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'TIMEBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TIMEBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TIMEBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'TPBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TPBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TPBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'HRMSBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HRMSBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%HRMSBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'WSETBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WSETBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WSETBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'SWLBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SWLBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SWLBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'WANGBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WANGBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WANGBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'NWAVE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NWAVE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NWAVE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NSURG'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NSURG'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NSURG
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NWIND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NWIND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NWIND
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NTIME'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NTIME'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NTIME
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'FREQMINBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQMINBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%FREQMINBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'FREQMAXBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQMAXBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%FREQMAXBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'FREQNUMBC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FREQNUMBC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%FREQNUMBC(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'HRMS'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HRMS'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%HRMS(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SIGMA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SIGMA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SIGMA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'H'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',H'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%H(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'WSETUP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WSETUP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WSETUP(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SIGSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SIGSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SIGSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'XBINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',XBINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%XBINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZBINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZBINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZBINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'FBINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FBINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%FBINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'XS'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',XS'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%XS(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'YLINE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',YLINE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%YLINE(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'DYLINE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DYLINE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DYLINE(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'AGLINE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',AGLINE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%AGLINE(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'NBINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NBINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%NBINP(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'DXD2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DXD2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DXD2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DXDX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DXDX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DXDX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DX2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DX2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DX2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'XB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',XB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%XB(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'ZB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZB(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'FB2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FB2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%FB2(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'SWLDEP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SWLDEP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%SWLDEP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'BSLOPE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BSLOPE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%BSLOPE(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'JMAX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JMAX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%JMAX(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'JSWL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JSWL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%JSWL(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'GRAV'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRAV'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%GRAV
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SQR2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SQR2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SQR2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SQR8'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SQR8'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SQR8
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'PI'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',PI'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%PI
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'TWOPI'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TWOPI'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%TWOPI
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SQRG1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SQRG1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SQRG1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SQRG2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SQRG2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SQRG2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WKP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WKP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WKP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'CP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%CP(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'WN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WN(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'WKPSIN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WKPSIN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WKPSIN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'STHETA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',STHETA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%STHETA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'CTHETA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CTHETA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%CTHETA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'FSX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FSX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FSX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'FSY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FSY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FSY
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'FE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%FE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'QWX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QWX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%QWX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'QWY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QWY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%QWY
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'GBX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GBX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%GBX(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'GBY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GBY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%GBY(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'GF'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GF'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%GF(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'GAMMA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GAMMA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%GAMMA
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'QBREAK'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QBREAK'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QBREAK(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'DBSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DBSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DBSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SISMAX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SISMAX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SISMAX
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ABREAK'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ABREAK'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%ABREAK(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'DVEGSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DVEGSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DVEGSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SXXSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SXXSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SXXSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'TBXSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TBXSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TBXSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SXYSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SXYSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SXYSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'TBYSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TBYSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TBYSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'EFSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EFSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%EFSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'DFSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DFSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DFSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'XR'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',XR'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%XR
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ZR'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZR'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ZR
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SSP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SSP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SSP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JR'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JR'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JR
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'UMEAN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UMEAN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UMEAN(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'USTD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',USTD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%USTD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'USTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',USTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%USTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VMEAN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VMEAN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VMEAN(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VSTD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VSTD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VSTD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'WF'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WF'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WF
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SG'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SG'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SG
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SPORO1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SPORO1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SPORO1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WFSGM1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WFSGM1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WFSGM1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'GSGM1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GSGM1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%GSGM1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'TANPHI'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TANPHI'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%TANPHI
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BSLOP1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BSLOP1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BSLOP1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BSLOP2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BSLOP2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BSLOP2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'EFFB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EFFB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%EFFB
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'EFFF'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EFFF'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%EFFF
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'D50'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',D50'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%D50
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SHIELD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SHIELD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SHIELD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'GSD50S'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GSD50S'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%GSD50S
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BLP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BLP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BLP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SLP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SLP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SLP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BLD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BLD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BLD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BEDLM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BEDLM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BEDLM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'CSTABN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CSTABN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%CSTABN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'CSEDIA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CSEDIA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%CSEDIA
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'PS'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',PS'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%PS(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VS'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VS'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VS(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QSX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QSX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QSX(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QSY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QSY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QSY(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'PB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',PB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%PB(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'GSLOPE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GSLOPE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%GSLOPE(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QBX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QBX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QBX(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QBY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QBY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QBY(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'Q'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',Q'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%Q(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VBX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VBX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VBX(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VSX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VSX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VSX(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VBY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VBY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VBY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VSY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VSY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VSY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'DZX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DZX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%DZX(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'DELT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DELT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DELT
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DELZB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DELZB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%DELZB(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'RBZERO'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RBZERO'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%RBZERO
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'RBETA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RBETA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RBETA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'RQ'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RQ'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RQ(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'RX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RX(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'RY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RY(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'RE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RE(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'XPINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',XPINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%XPINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZPINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZPINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZPINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'HP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%HP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'WNU'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WNU'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WNU
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SNP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SNP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SNP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SDP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SDP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SDP
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ALPHA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ALPHA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ALPHA
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BETA1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BETA1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BETA1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BETA2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BETA2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BETA2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ALSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ALSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ALSTA
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BESTA1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BESTA1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BESTA1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BESTA2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BESTA2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BESTA2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'UPMEAN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UPMEAN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UPMEAN(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'UPSTD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UPSTD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UPSTD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'DPSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DPSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DPSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QP(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'UPMWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UPMWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UPMWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'NPINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NPINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%NPINP(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'RWH'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RWH'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%RWH
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'RCREST'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RCREST'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%RCREST(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'QO'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QO'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QO(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'QOTF'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QOTF'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%QOTF
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SPRATE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SPRATE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SPRATE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SLPOT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SLPOT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%SLPOT
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JCREST'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JCREST'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) (cg%JCREST(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'W10'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',W10'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%W10(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'WANGLE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WANGLE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WANGLE(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'WINDCD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WINDCD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%WINDCD(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'TWXSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TWXSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TWXSTA(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'TWYSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TWYSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TWYSTA(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'AWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',AWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%AWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WDN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WDN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WDN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'EWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%EWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'CWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%CWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'AQWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',AQWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%AQWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'AGWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',AGWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%AGWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'AUWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',AUWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%AUWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WPM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WPM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%WPM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ALSTA2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ALSTA2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ALSTA2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BE2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BE2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BE2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'BE4'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',BE4'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%BE4
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'PWET'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',PWET'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%PWET(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'USWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',USWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%USWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'HWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%HWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'SIGWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SIGWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SIGWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'UMEAWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UMEAWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UMEAWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'USTDWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',USTDWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%USTDWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VMEAWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VMEAWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VMEAWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VSTDWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VSTDWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VSTDWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'HEWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HEWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%HEWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'UEWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UEWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%UEWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'QEWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QEWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%QEWD(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'H1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',H1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%H1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JWD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JWD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JWD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JDRY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JDRY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JDRY
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'SWLAND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',SWLAND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%SWLAND(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'ISWLSL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ISWLSL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ISWLSL
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JSL'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JSL'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JSL
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JSL1'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JSL1'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JSL1
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'IOFLOW'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',IOFLOW'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%IOFLOW
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'HWDMIN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HWDMIN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%HWDMIN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NPT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NPT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NPT
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NPE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NPE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NPE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ZW'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZW'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%ZW
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'QD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%QD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'QM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',QM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%QM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JXW'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JXW'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JXW
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'JX2'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',JX2'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%JX2
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'NOPOND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NOPOND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NOPOND
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DETADY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DETADY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DETADY(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'DSWLDT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DSWLDT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DSWLDT(i),i=1,30000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 30000

        write(99,'(a)',IOSTAT=s) 'TSQO'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TSQO'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TSQO(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'TSQBX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TSQBX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TSQBX(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'TSQSX'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TSQSX'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%TSQSX(i),i=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 100

        write(99,'(a)',IOSTAT=s) 'VEGCD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGCD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%VEGCD
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'VEGCDM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGCDM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%VEGCDM
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'VEGN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGN(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGB(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGD(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGH'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGH'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGH(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGFB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGFB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGFB(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGRD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGRD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGRD(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGRH'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGRH'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGRH(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGZD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGZD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGZD(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VEGZR'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VEGZR'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%VEGZR(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'UPROOT'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',UPROOT'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%UPROOT(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'EDIKE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EDIKE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%EDIKE(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZB0'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZB0'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZB0(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'DSTA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DSTA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DSTA(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'DSUM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DSUM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%DSUM(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'GDINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GDINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GDINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'GRINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GRINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'GRDINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRDINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GRDINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'GRSD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRSD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GRSD(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'GRSR'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRSR'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GRSR(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'GRSRD'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',GRSRD'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%GRSRD(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'DEEB'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DEEB'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DEEB
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'DEEF'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DEEF'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DEEF
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'WMINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WMINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%WMINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'WMNODE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',WMNODE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%WMNODE(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZMESH'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZMESH'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZMESH(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZBSTON'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZBSTON'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZBSTON(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZPSTON'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZPSTON'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZPSTON(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'HPSTON'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',HPSTON'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%HPSTON(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'VDSAND'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VDSAND'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VDSAND(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'CPSTON'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',CPSTON'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%CPSTON
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'ISTSAN'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ISTSAN'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%ISTSAN
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'EPCLAY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',EPCLAY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%EPCLAY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'ZP0'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',ZP0'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%ZP0(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'RCINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RCINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%RCINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'FCINP'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FCINP'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%FCINP(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'RCLAY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RCLAY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%RCLAY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'FCLAY'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',FCLAY'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s)
     +    ((cg%FCLAY(i,j),i=1,5000),j=1,100)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000 * 100

        write(99,'(a)',IOSTAT=s) 'DIKETOE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',DIKETOE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%DIKETOE
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'TZ'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',TZ'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%TZ
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'RUNUPKAPPA'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RUNUPKAPPA'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%RUNUPKAPPA
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'RUNUPPHI'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',RUNUPPHI'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) cg%RUNUPPHI
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        write(99,'(a)',IOSTAT=s) 'VMEASOMEG'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VMEASOMEG'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VMEASOMEG(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VMEASSE'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VMEASSE'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VMEASSE(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'VMEASWNUM'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',VMEASWNUM'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(d20.10)',IOSTAT=s) (cg%VMEASWNUM(i),i=1,5000)
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 5000

        write(99,'(a)',IOSTAT=s) 'NMEASSPEC'
        if (0 < s) then
          s = 3
          return
        endif
        l = l + 1
        write(98,'(i0,a)',IOSTAT=s) l, ',NMEASSPEC'
        if (0 < s) then
          s = 4
          return
        endif
        write(99,'(i0)',IOSTAT=s) cg%NMEASSPEC
        if (0 < s) then
          s = 5
          return
        endif
        l = l + 1

        close(99,IOSTAT=s)
        if (0 < s) then
          s = 6
          return
        endif
        close(98,IOSTAT=s)
        if (0 < s) then
          s = 7
          return
        endif

        s = 0
      end function dump_vars

c------------------------------------------------------------------------
c------------------------------------------------------------------------
      ! Initializes the model with default hardcoded values.
      subroutine initialize_from_defaults(model)
        type (cshore_vars_type), intent (out) :: model

        model%alpha = 0.75
        model%t_end = 20.
        model%n_x = 10
        model%n_y = 20
        call initialize(model)
      end subroutine initialize_from_defaults

      ! Allocates memory and sets values for either initialization technique.
      subroutine initialize(model)
        type (cshore_vars_type), intent (inout) :: model

        model%id = 0
        model%t = 0.
        model%dt = 1.
        model%dx = 1.
        model%dy = 1.

        allocate(model%temperature(model%n_y, model%n_x))
        allocate(model%temperature_tmp(model%n_y, model%n_x))

        model%temperature = 0.
        model%temperature_tmp = 0.

        call set_boundary_conditions(model%temperature)
        call set_boundary_conditions(model%temperature_tmp)
      end subroutine initialize

      ! Sets boundary conditions on values array.
      subroutine set_boundary_conditions(z)
        implicit none
        real, dimension (:,:), intent (out) :: z
        integer :: i, top_x

        top_x = size(z, 2)-1

        do i = 0, top_x
           z(1,i+1) = 0.25*top_x**2 - (i - 0.5*top_x)**2
        end do
      end subroutine set_boundary_conditions

      ! Frees memory when program completes.
      subroutine cleanup(model)
        type (cshore_vars_type), intent (inout) :: model
        integer :: file_unit, status
        logical :: file_opened
        integer :: output_file_unit_range_begin = 20
        integer :: output_file_unit_range_end = 40

c        deallocate (model%ZB, stat = status)
        deallocate (model%MAXMWL, stat = status)

        ! close all output files
        if (model%enable_cshore_outputfiles) then
          do file_unit =
     +        output_file_unit_range_begin,
     +        output_file_unit_range_end,
     +        1
            inquire (unit = file_unit, opened = file_opened)
            if (file_opened) then
              close (file_unit, iostat = status)
            end if
          end do
        end if

      end subroutine cleanup

      ! Steps the heat model forward in time.
      subroutine advance_in_time(model)
        type (cshore_vars_type), intent (inout) :: model

        call solve_2d(model)
        model%temperature = model%temperature_tmp
        model%t = model%t + model%dt
      end subroutine advance_in_time

      ! The solver for the two-dimensional heat equation.
      subroutine solve_2d(model)
        type (cshore_vars_type), intent (inout) :: model

        real :: dx2
        real :: dy2
        real :: coef
        integer :: i, j

        dx2 = model%dx**2
        dy2 = model%dy**2
        coef = model%alpha * model%dt / (2. * (dx2 + dy2))

        do j = 2, model%n_x-1
           do i = 2, model%n_y-1
              model%temperature_tmp(i,j) =
     +             model%temperature(i,j) + coef * (
     +             dx2*(model%temperature(i-1,j) +
     + model%temperature(i+1,j)) +
     +             dy2*(model%temperature(i,j-1) +
     + model%temperature(i,j+1)) -
     +             2.*(dx2 + dy2)*model%temperature(i,j) )

           end do
        end do
      end subroutine solve_2d

      ! A helper routine for displaying model parameters.
      subroutine print_info(model)
        type (cshore_vars_type), intent (in) :: model

        write(*,"(a10, i8)") "n_x:", model%n_x
        write(*,"(a10, i8)") "n_y:", model%n_y
        write(*,"(a10, f8.2)") "dx:", model%dx
        write(*,"(a10, f8.2)") "dy:", model%dy
        write(*,"(a10, f8.2)") "alpha:", model%alpha
        write(*,"(a10, f8.2)") "dt:", model%dt
        write(*,"(a10, f8.2)") "t:", model%t
        write(*,"(a10, f8.2)") "t_end:", model%t_end
      end subroutine print_info

      ! A helper routine that prints the current state of the model.
      subroutine print_values(model)
        type (cshore_vars_type), intent (in) :: model
        integer :: i, j
        character(len=30) :: rowfmt

        write(rowfmt,'(a,i4,a)') '(', model%n_x, '(1x,f6.1))'
        do i = 1, model%n_y
           write(*,fmt=rowfmt) model%temperature(i,:)
        end do
      end subroutine print_values

      end module cshore_module
