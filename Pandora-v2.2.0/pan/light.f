      subroutine LIGHT
     $(X,IX,W,IW,SFERE,LEGEND,LYNC,WLYNC,XLYNC)
C
C     Rudolf Loeser, 1968 Jul 15
C---- Supervises the line emission calculation.
C     (This is version 2 of LIGHT.)
C     !DASH
      save
C     !DASH
      real*8 W, WLYNC, X, XLYNC, dummy
      integer I, IBCT, IC, ICPT, IFDL, IFO, IJECT, IKVL, ILB1, ILB2,
     $        ILB3, IMG, IN, IQANA, IQCFX, IQFLW, IQIGR, IS, ISST, IST,
     $        ITCA, ITCI, ITFL, IVEL, IW, IWS, IX, JN, JSAV, KLAB, KLNC,
     $        LF, LINK, LUA, LUG, LYNC, MOX, MUX, N, NO, NT, NVX, jummy
      logical CONFLX, CONINT, DOPROF, ECSPEC, LEGEND, LINFLX, LININT,
     $        SFERE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(19),LF )
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
      equivalence (IQQ(  1),IQANA)
      equivalence (IQQ( 44),IQIGR)
      equivalence (IQQ(335),IQFLW)
      equivalence (IQQ( 80),IQCFX)
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
      external KITTY, ZEUS, LEANDRA, LIDGET, BLACK, WGIVE, IGIVE, ZORA,
     $         TAUPO, GADUMI, ERNEST, NARADEL, ISIDORE, RYMON, MALLARD,
     $         SIAM, PET, OPAL, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               WLYNC(Klynf), XLYNC(Klynf)
      dimension WLYNC(*),     XLYNC(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),ILB1  ),(IN( 2),IST   ),(IN( 3),ISST  ),(IN( 4),IBCT  ),
     $(IN( 5),ICPT  ),(IN( 6),IVEL  ),(IN( 7),IFO   ),(IN( 8),IFDL  ),
     $(IN( 9),ITCI  ),(IN(10),ITCA  ),(IN(11),ITFL  ),(IN(12),ILB2  ),
     $(IN(13),ILB3  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),IMG   ),(JN( 2),IKVL  )
C
      data CONINT,CONFLX,ECSPEC,KLNC,KLAB /3*.false., 0, 4/
C
      call HI ('LIGHT')
C     !BEG
C---- Set up parameters in LOOPER (needed for LEANDRA)
      SPHERE = SFERE
      FLOBRD = IQFLW.gt.0
C---- Get LFBV, number of viewing positions (frontface/backface)
      call ZORA    (LFBV)
C
C     (Get, and allocate, W & IW allotments)
      call LEANDRA (IN, IS,  MOX, 'LIGHT')
      call NARADEL (JN, IWS, MUX, 'LIGHT')
C
      LININT = .not.SPHERE
      LINFLX = (LF.gt.1).or.(SPHERE.and.(IQCFX.gt.0))
C---- Get JSAV, = Spectrum-Summary switch
      call KITTY   (SPHERE, JSAV)
C---- Set up output unit for Profile Analyses and for graphs
      call ZEUS    (NO, IQANA, LUA)
      call ZEUS    (NO, IQIGR, LUG)
C---- Set up velocity set
      call GADUMI  (X, IX, W, W(IVEL), IW(IKVL), NO)
C---- (?  Dump line data)
      call MALLARD ('LIGHT')
C     !EJECT
C---- Loop over all transitions
      do 101 I = 1,NT
        call PET       (I)
        call ISIDORE   (DOPROF)
        if(DOPROF) then
C----     Write transition header
          call ERNEST  (NO, KLAB, IJECT)
C----     Read line intensity data blocks
          call LIDGET  (W(ILB1), 1, W(ILB2), 1, W(ILB3), 1, I)
C----     Initialize Spectrum Save File for this transition
          call RYMON   (W(ILB1), W(ILB2))
C----     Compute line background intensity/flux
          do 100 IC = 1,2
            call TAUPO (IC, LINK)
            call SIAM  (X, IX, W, IW, SPHERE, CONINT, CONFLX, ECSPEC,
     $                  LININT, LINFLX, W(ILB3), LEGEND, LINK, KLNC,
     $                  jummy, dummy, dummy, IJECT)
  100     continue
C
C----     Compute
          call BLACK   (X, IX, W, IW, JSAV, LEGEND, NO, W(ILB1),
     $                  W(ILB2), W(ILB3), W(IST), W(ISST), W(IBCT),
     $                  W(ICPT), W(IVEL), IW(IKVL), W(IFDL), W(ITCI),
     $                  W(ITCA), W(ITFL), IW(IMG), W(IFO), LYNC, WLYNC,
     $                  XLYNC, LUA, LUG, IJECT)
        end if
  101 continue
C---- (?  Write H Ly normalization factors to .msc)
      call OPAL        (X)
C
C     (Give back W & IW allotments)
      call WGIVE       (W,  'LIGHT')
      call IGIVE       (IW, 'LIGHT')
C     !END
      call BYE ('LIGHT')
C
      return
      end
