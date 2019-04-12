      subroutine GRIS
     $(K,STRN,BCTRN,COPTRN,VEL,KVEL,FDDL,GTN,DP,DW,DDL,CDL,DL,ISB1,V,
     $ VR,TE,Z,FRR,XNE,MPROM,W,IW)
C
C     Rudolf Loeser, 1980 Jun 11
C---- Computes Line and Flux profiles in spherical coordinates.
C     (This is version 3 of GRIS.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, FDDL, FRR, GTN, R1N,
     $       STRN, TE, V, VEL, VR, W, XNE, Z
      integer IAD, IAS, IDF, IDFC, IDI, IDII, IEPC, IEPD, IEPS, IEW,
     $        IFDL, IN, IQEGR, IQESD, IS, ISB1, ISF, ISFC, ISI, ISII,
     $        IT1D, IT1S, ITF, IVX, IW, IWS, IWTAB, IWVN, IXHZ, IYDL,
     $        JN, K, KVEL, MOX, MPROM, MUX, N1D, N1S, NO, NOG
      logical DUMP
      character QNAME*8
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  1),QNAME)
      equivalence (RZQ( 23),R1N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
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
      equivalence (IQQ( 40),IQESD)
      equivalence (IQQ(162),IQEGR)
C     !DASH
C     !EJECT
      external ISHTAR, ZEUS, LISA, IGIVE, RIGORD, SNUGUD, WGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               STRN(N,KM), BCTRN(N,KM), COPTRN(N,KM), FDDL(N), XNE(N),
      dimension STRN(*),    BCTRN(*),    COPTRN(*),    FDDL(*), XNE(*),
C
C               KVEL(NVEL), DP(N,LDLMX), DW(N), DDL(LDLMX), CDL(LDLMX),
     $          KVEL(*),    DP(*),       DW(*), DDL(*),     CDL(*),
C
C               DL(KM), V(N), VEL(N,NVEL), VR(N), TE(N), GTN(N), Z(N),
     $          DL(*),  V(*), VEL(*),      VR(*), TE(*), GTN(*), Z(*),
C
C               FRR(MRR)
     $          FRR(*)
C
      dimension IN(23)
      equivalence
     $(IN( 1),IEPC  ),(IN( 2),ISI   ),(IN( 3),IEPS  ),(IN( 4),IDI   ),
     $(IN( 5),ISF   ),(IN( 6),IDF   ),(IN( 7),ITF   ),(IN( 8),IAS   ),
     $(IN( 9),IAD   ),(IN(10),IVX   ),(IN(11),IXHZ  ),(IN(12),IFDL  ),
     $(IN(13),IEPD  ),(IN(14),IWVN  ),(IN(15),IT1S  ),(IN(16),ISFC  ),
     $(IN(17),IYDL  ),(IN(18),IT1D  ),(IN(19),IDFC  ),(IN(20),ISII  ),
     $(IN(21),IDII  ),(IN(22),IEW   ),(IN(23),IWTAB )
C
      dimension JN(2)
      equivalence
     $(JN( 1),N1S   ),(JN( 2),N1D   )
C
      call HI ('GRIS')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ISHTAR (IN, IS,  MOX, 'GRIS' )
      call RIGORD (JN, IWS, MUX, 'GRIS' )
C
      call ZEUS   (NO, IQEGR, NOG)
      SPHERE = .true.
      DUMP   = IQESD.gt.0
C
      call SNUGUD (K, DL, WVLTRN, W(IWVN), W(IWTAB), 1)
      call LISA   (NO, NOG, DUMP, K, Z, GTN, COPTRN, STRN, BCTRN, DP,
     $             DW, XNE, MPROM, DDL, FDDL, CDL, FRR, W(ISI),
     $             W(IEPC), W(IEPS), R1N, QNAME, V, VR, VEL, KVEL, TE,
     $             W(IDI), W(ISF), W(IDF), W(ITF), W(IAS), W(IAD),
     $             W(IVX), DL, W(IEPD), IW(N1S), W(IT1S), IW(N1D),
     $             W(IT1D), W(ISFC), W(IDFC), W, IW, W(ISII), W(IDII),
     $             W(IEW), W(IXHZ), W(IFDL), W(IYDL), ISB1, W(IWVN),
     $             W(IWTAB))
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'GRIS')
      call IGIVE  (IW, 'GRIS')
C     !END
      call BYE ('GRIS')
C
      return
      end
