      subroutine GREY
     $(JSAV,LEGEND,NO,K,QNAME,STRN,SSTRN,BCTRN,COPTRN,TCINT,TCINTA,
     $ TCFLX,VEL,KVEL,FDDL,LDL,DDL,CDL,DP,DW,GTN,GTNS,CNDT,DL,PROGLI,
     $ PGD,ISB1,Z,TE,V,VR,EMU,EMUF,WMUF,XNE,WTP,MPROM,X,W,IW,LYNC,
     $ WLYNC,XLYNC,LUA,LUG,IJECT)
C
C     Rudolf Loeser, 1977 Jan 28
C---- Controls calculation, printing and plotting of Intensity and Flux
C     profiles for Transition (IU/IL), using plane-parallel coordinates.
C     (This is version 2 of GREY.)
C     (Completely revised 2000 Jul 25)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, CNDT, COPTRN, DDL, DL, DP, DW, EMU, EMUF, FDDL,
     $       GTN, GTNS, PGD, PROGLI, SSTRN, STRN, TCFLX, TCINT, TCINTA,
     $       TE, V, VEL, VR, W, WLYNC, WMUF, WTP, X, XLYNC, XNE, Z
      integer IINT, IINTA, IINTFB, IINTL, IJECT, IN, IPRO, IPROL, IQLTE,
     $        IQVSW, IS, ISB1, ISNU, ISNUL, ITNU, ITNUL, IVEX, IW,
     $        IWINT, IWINTL, IWTAB, IWVNUM, JSAV, K, KVEL, L, LDL, LF,
     $        LU, LUA, LUG, LYNC, MOX, MPROM, N, NO, NUA, NUG
      logical FLX, INCRAD, KILROY, LEGEND, LTE, VSW
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
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
      equivalence (IQQ( 33),IQLTE)
      equivalence (IQQ(173),IQVSW)
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
      external MAURILA, DAMSEL, SWEET, DIPRON, DOROTHY, DESMAN, PERROS,
     $         ROT, RED, PERCY, SNUGUD, WGIVE, DOWNY, BLOAT, ROXY, REX,
     $         HERA, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               EMUF(LF), STRN(N,KM), TCINT(KM,L,LFBV), TCFLX(KM,LFBV),
      dimension EMUF(*),  STRN(*),    TCINT(*),         TCFLX(*),
C
C               SSTRN(N,KM), BCTRN(N,KM), COPTRN(N,KM), XNE(N), EMU(L),
     $          SSTRN(*),    BCTRN(*),    COPTRN(*),    XNE(*), EMU(*),
C
C               DP(N,LDLMX), FDDL(N), PGD(3,LDLMX), DDL(LDLMX), GTN(N),
     $          DP(*),       FDDL(*), PGD(*),       DDL(*),     GTN(*),
C
C               CDL(LDLMX), V(N), CNDT(N), Z(N), DW(N), TE(N), GTNS(N),
     $          CDL(*),     V(*), CNDT(*), Z(*), DW(*), TE(*), GTNS(*),
C
C               VEL(N,NVEL), TCINTA(KM,LFBV), WMUF(LF), DL(KM), VR(N),
     $          VEL(*),      TCINTA(*),       WMUF(*),  DL(*),  VR(*),
C
C               KVEL(NVEL), WTP(NVX), WLYNC(KLYNF), XLYNC(KLYNF)
     $          KVEL(*),    WTP(*),   WLYNC(*),     XLYNC(*)
C
      dimension IN(15)
      equivalence
     $(IN( 1),IWVNUM),(IN( 2),ISNU  ),(IN( 3),ISNUL ),(IN( 4),ITNU  ),
     $(IN( 5),ITNUL ),(IN( 6),IINT  ),(IN( 7),IINTL ),(IN( 8),IINTA ),
     $(IN( 9),IPRO  ),(IN(10),IPROL ),(IN(11),IVEX  ),(IN(12),IWINT ),
     $(IN(13),IWINTL),(IN(14),IWTAB ),(IN(15),IINTFB)
C
      call HI ('GREY')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAURILA (IN, IS, MOX, 'GREY')
C
C---- Set up tables of WVNUM and WTAB corresponding to DL
      call SNUGUD  (K, DL, WVLTRN, W(IWVNUM), W(IWTAB), 1)
C---- Other initialization
      SPHERE = .false.
      KILROY = .true.
      LTE    = IQLTE.gt.0
      FLX    = LF.gt.1
      VSW    = IQVSW.gt.0
C     !EJECT
C---- Loop over expansion velocities.
      do 102 NVY = 1,NVEL
C----   Set up current "expansion velocity"
        call SWEET        (N, VEL, KVEL, W(IVEX))
C----   Set up output units
        call ROXY         (NO,LUA,LUG,LU,NUA,NUG)
C
C----   Loop over viewing positions
        do 101 LFB = 1,LFBV
C
C----     Loop over Look-angles.
          do 100 MF = 1,LF
C----       Set up the current value of look-angle, EMOO, and
C           set MUK .gt. 0 if line intensity profile must be printed.
            call DOROTHY  (EMUF, EMU, L)
C----       Set up incident radiation flag
            call DOWNY    (LFB, EMOO, INCRAD)
C
            if((MF.eq.1).or.VSW.or.EXPAND) then
C----         Compute monochromatic TAU and S, and absorption profile
              call DIPRON (W, IW, N, K, LDL, VSW, LTE, WVLTRN, DL, Z,
     $                     TE, V, VR, W(IVEX), DP, DW, XNE, MPROM,
     $                     DDL, FDDL, CDL, COPTRN, BCTRN, GTN, GTNS,
     $                     STRN, SSTRN, W(ITNU), W(ITNUL), W(ISNU),
     $                     W(ISNUL), LU, NUA)
            end if
C----       Compute, print, and analyze intensity profile /Hz
            call PERROS   (W, IW, N, K, L, LDL, KILROY, LTE, INCRAD,
     $                     LEGEND, JSAV, LU, IJECT, NUG, DL,
     $                     W(IWVNUM), W(IWTAB), Z, TE, CNDT, TCINT,
     $                     TCINTA, ISB1, W(ITNU), W(ITNUL), W(ISNU),
     $                     W(ISNUL), W(IINT), W(IINTL), W(IINTA),
     $                     W(IPRO), W(IPROL), DDL, PROGLI, PGD)
C----       Accumulate profile of this MU into weighted sum for flux
            call PERCY    (FLX, LTE, K, W(IPRO), W(IPROL), WMUF,
     $                     W(IWINT), W(IWINTL))
C----       (?  H Ly normalization factor)
            call HERA     (X, W, LYNC, WLYNC, XLYNC, K,WVLTRN, DL,
     $                     W(IINT))
  100     continue
C         End of loop over look angles
C
          if(FLOBRD.and.(LFB.eq.1)) then
C----       Accumulate front-face NLTE profiles for this velocity
C           into flow-broadened profiles
            call REX       (K, LF, WTP, W(IINT), W(IINTFB))
          end if
C     !EJECT
C----     Plot absolute intensities, non-LTE.
          call ROT    (NUG, K, LF, W(IWTAB), W(IINT), INCRAD, W(IINTA),
     $                 PROGLI)
C----     Save checksum of Intensity/Hz (non-LTE)
          call DAMSEL (W(IINT), K, LF)
C
C----     Compute and print flux profile
          call RED    (W, FLX, LF, K, LTE, EMUF, WVLTRN, DL, W(IWVNUM),
     $                 W(IWTAB), W(IWINT), W(IWINTL), TCFLX, PROGLI,
     $                 LDL, DDL, PGD, ISB1, KILROY, LU, IJECT, NUG)
  101   continue
C       End of loop over viewing positions
C
  102 continue
C     End of loop over velocities
C
      if(FLOBRD) then
        call BLOAT    (K, L, LF, FLX, WVLTRN, DL, W(IWVNUM), W(IWTAB),
     $                 EMU, EMUF, WMUF, W(IINTFB), TCINT, TCFLX,
     $                 PROGLI, LDL, DDL, PGD, NO, LUG, IJECT, W, IW)
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'GREY')
C     !END
      call BYE ('GREY')
C
      return
      end
