      subroutine FABLE
     $(NO,KFUNC,KNLTE,KSTAR,EMUF,LF,WVL,DL,WVNUM,WTAB,K,FHZ,FIN,FIS,
     $ FAN,BT,YY,MUX,MYX,KODE,RES,TF,SF,KLIN,TC,KTC,KILROY,LDL,DDL,
     $ ISB1,IJECT)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Controls output of Line Profiles.
C
C     KFUNC =1 means Intensity,  =2 means Flux.
C     KNLTE =1 means NON-LTE,  =0 means LTE.
C     KSTAR =1 means incident radiation included,  =0 means not.
C
C     (This is version 2 of FABLE.)
C     !DASH
      save
C     !DASH
      real*8 BT, DDL, DL, EMUF, FAN, FHZ, FIN, FIS, RES, SF, TC, TF,
     $       WTAB, WVL, WVNUM, YY
      integer IJECT, IQPPU, ISB1, K, KFUNC, KLIN, KNLTE, KODE, KSTAR,
     $        KTC, LDL, LF, MUX, MYX, NO
      logical KILROY
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(151),IQPPU)
C     !DASH
      external BERRY, ARUNDEL, BAFFLE, OBIZI, HI, BYE
C
C               WVNUM(KM), RES(KM), YY(KM), MUX(KM), KODE(KM), FIN(KM),
      dimension WVNUM(*),  RES(*),  YY(*),  MUX(*),  KODE(*),  FIN(*),
C
C               FIS(KM), FAN(KM), FHZ(KM), WTAB(KM), MYX(KM), EMUF(LF),
     $          FIS(*),  FAN(*),  FHZ(*),  WTAB(*),  MYX(*),  EMUF(*),
C
C               TC(KM), DL(KM), SF(KM), TF(KM), BT(KM), DDL(LDLMX)
     $          TC(*),  DL(*),  SF(*),  TF(*),  BT(*),  DDL(*)
C     !EJECT
C
      call HI ('FABLE')
C     !BEG
      if(NO.gt.0) then
C----   Output detailed heading
        call BERRY    (NO, KFUNC, KNLTE, KSTAR, WVL, IJECT, ISB1)
C----   Output profile
        call ARUNDEL  (NO, KFUNC, RES, WTAB, K, KTC, YY, MUX, MYX,
     $                 KODE, FIN, FIS, FAN, FHZ, BT, TF, SF, KNLTE)
        if(KFUNC.eq.1) then
C----     Print final messages
          call BAFFLE (NO)
        end if
      end if
      if((IQPPU.gt.0).and.(KNLTE.eq.1)) then
C----   Save profile data in Special Spectrum Save File
        call OBIZI    (KFUNC, KSTAR, WVL, K, KLIN, KILROY, SPHERE,
     $                 DL, WVNUM, TC, FAN, FHZ, BT, MUX, MYX, YY,
     $                 KODE, TF, SF, LDL, DDL)
      end if
C     !END
      call BYE ('FABLE')
C
      return
      end
