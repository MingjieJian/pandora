      subroutine PERROS
     $(W,IW,N,K,L,LDL,KILROY,LTE,INCRAD,LEGEND,JSAV,NO,IJECT,LUG,DL,
     $ WVNUM,WTAB,Z,TE,CNDT,TCINT,TCINTA,ISB1,TNU,TNUL,SNU,SNUL,YHZ,
     $ YHZL,YHZA,ZHZ,ZHZL,DDL,PROGLI,PGD)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Computes and prints emergent line profile /Hz.
C     !DASH
      save
C     !DASH
      real*8 CNDT, DDL, DL, PGD, PROGLI, SNU, SNUL, TCINT, TCINTA, TE,
     $       TNU, TNUL, W, WTAB, WVNUM, YHZ, YHZA, YHZL, Z, ZHZ, ZHZL
      integer ICNTA, ICNTI, IJECT, IKODE, IMUX, IMYX, IN, INDL, IQDIL,
     $        IQPPU, IRESA, IRESL, IRESN, IS, ISB1, ITMUN, IVEC, IW,
     $        IWS, IYY, JN, JSAV, K, KRCA, KRCL, KRCN, KTA, KTI, L, LDL,
     $        LUG, MOX, MUX, N, NO
      logical DODIDH, INCRAD, KILROY, LEGEND, LTE, SVDIDH, lummy
C     !COM
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
      equivalence (IQQ(289),IQDIL)
      equivalence (IQQ(151),IQPPU)
C     !EJECT
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
C     !DASH
C     !EJECT
      external ZOE, ZIRMA, FEDORA, NIGRA, NORGA, ORNATI, WANAKA, WGIVE,
     $         IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               TCINT(KM,L,LFBV), TCINTA(KM,LFBV), TNUL(N,KM), CNDT(N),
      dimension TCINT(*),         TCINTA(*),       TNUL(*),    CNDT(*),
C
C               TNU(N,KM), SNU(N,KM), WVNUM(KM), PGD(3,LDLMX), ZHZ(KM),
     $          TNU(*),    SNU(*),    WVNUM(*),  PGD(*),       ZHZ(*),
C
C               YHZ(KM,LF), YHZL(KM), YHZA(KM), ZHZL(KM), DL(KM), Z(N),
     $          YHZ(*),     YHZL(*),  YHZA(*),  ZHZL(*),  DL(*),  Z(*),
C
C               DDL(LDLMX), SNUL(N,KM), WTAB(KM), TE(N)
     $          DDL(*),     SNUL(*),    WTAB(*),  TE(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),ICNTI ),(IN( 2),ICNTA ),(IN( 3),IRESA ),(IN( 4),IYY   ),
     $(IN( 5),ITMUN ),(IN( 6),IRESN ),(IN( 7),IRESL ),(IN( 8),IVEC  )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IMUX  ),(JN( 2),IKODE ),(JN( 3),IMYX  ),(JN( 4),INDL  )
C     !EJECT
C
      call HI ('PERROS')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call NORGA    (IN, IS , MOX, 'PERROS')
      call NIGRA    (JN, IWS, MUX, 'PERROS')
C
      DODIDH = IQDIL.gt.0
      SVDIDH = IQPPU.gt.0
C---- Get background data (for residuals calculation)
      call ZOE      (INCRAD, L, K, TCINT, TCINTA, W(ICNTI), KTI,
     $               W(ICNTA), KTA)
C---- Get table of line core indices
      call WANAKA   (DDL, LDL, DL, K, IW(INDL))
C---- Compute and print non-LTE line profile
      call ZIRMA    (W, IW, N, K, 1, TNU , SNU , YHZ , IW(IMUX),
     $               W(IYY), IW(IKODE), IW(IMYX), W(ITMUN), WVLTRN,
     $               DL, DDL, IW(INDL), LDL, WVNUM, WTAB, W(IRESN),
     $               KRCN, ZHZ, W(ICNTI), KTI, KILROY, Z, TE, ISB1,
     $               DODIDH, SVDIDH, JSAV, LEGEND, W(IVEC), NO,
     $               IJECT)
C---- Repeat, with incident radiation.
      if(INCRAD) then
        call FEDORA (W, N, K, WVLTRN, DL, DDL, LDL, WVNUM, WTAB,
     $               W(ITMUN), CNDT, YHZ, YHZA, JSAV, IW(IMUX),
     $               W(IYY), IW(IMYX), IW(IKODE), NO, W(IRESA), KRCA,
     $               W(ICNTA), KTA, KILROY, ISB1, IJECT)
      end if
C---- Compute and print LTE line profile
      if(LTE) then
        call ZIRMA  (W, IW, N, K, 0, TNUL, SNUL, YHZL, IW(IMUX),
     $               W(IYY), IW(IKODE), IW(IMYX), W(ITMUN), WVLTRN,
     $               DL, DDL, IW(INDL), LDL, WVNUM, WTAB, W(IRESL),
     $               KRCL, ZHZL, W(ICNTI), KTI, KILROY, Z, TE, ISB1,
     $               DODIDH, .false., JSAV, lummy, W(IVEC), NO,
     $               IJECT)
      end if
      if(KRCN.gt.0) then
C----   Plot residual intensity.
        call ORNATI (W, K, LDL, WVLTRN, DL, WVNUM, WTAB, PROGLI, DDL,
     $               PGD, W(IRESN), KRCN, W(IRESL), KRCL, W(IRESA),
     $               KRCA, LUG)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W , 'PERROS')
      call IGIVE    (IW, 'PERROS')
C     !END
      call BYE ('PERROS')
C
      return
      end
