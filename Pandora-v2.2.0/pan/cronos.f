      subroutine CRONOS
     $(X,W,IW,TMU,Z,N,Y,MOVING,WN,WH,ILFLX,TITLE)
C
C     Rudolf Loeser, 1981 May 07
C---- Computes GR-weight matrices in plane-parallel geometry.
C     (This is version 2 of CRONOS.)
C     !DASH
      save
C     !DASH
      real*8 TMU, W, WH, WN, X, Y, Z
      integer ILFLX, IN, IQFIN, IQWDD, IS, ITAU, IW, IWHM, IWNM, JJCMU,
     $        JJGDT, JJGDZ, JJWMU, KGDT, LG, MOX, N
      logical DUMP, FINITE, MOVING
      character LABEL*27, TITLE*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(34),LG )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(131),JJCMU)
      equivalence (IZOQ(167),JJWMU)
      equivalence (IZOQ(190),JJGDZ)
      equivalence (IZOQ(191),JJGDT)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(46),KGDT )
C     !EJECT
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
      equivalence (IQQ(146),IQWDD)
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external MERU, GENGHIS, IOTA, UPSILON, MESHED, MASHED, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TMU(N,LG), Z(N), WN(N,N), WH(N,N)
      dimension TMU(*),    Z(*), WN(*),   WH(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWNM  ),(IN( 2),IWHM  ),(IN( 3),ITAU  )
C
      data LABEL /' after Geometrical Dilution'/
C     !EJECT
C
      call HI ('CRONOS')
C     !BEG
C     (Get, and allocate, W allotment)
      call MERU      (IN, IS, MOX, 'CRONOS', N)
C
      FINITE = IQFIN.gt.0
      DUMP   = IQWDD.gt.0
      if(DUMP) then
        call MESHED  ('CRONOS', 2)
      end if
C
C---- Compute basic weight matrices WN (Lambda-minus-one) and WH (Phi)
      call GENGHIS   (TMU, N, X(JJCMU), LG, FINITE, Y, MOVING, WN,
     $                X(JJWMU), WH, ILFLX, W(IWNM), W(IWHM), W(ITAU),
     $                W, IW, DUMP)
      if(DUMP) then
        call IOTA    (WN, N, TITLE, 'Integrated WN')
      end if
C---- Apply Geometrical Dilution
      call UPSILON   (Z, N, X(JJGDZ), X(JJGDT), WN, TITLE)
      if((KGDT.eq.1).and.DUMP) then
        call IOTA    (WN, N, TITLE, 'Integrated WN'//LABEL)
      end if
C
      if(ILFLX.gt.0) then
        if(DUMP) then
          call IOTA  (WH, N, TITLE, 'Integrated WH')
        end if
C----   Apply Geometrical Dilution
        call UPSILON (Z, N, X(JJGDZ), X(JJGDT), WH, TITLE)
        if((KGDT.eq.1).and.DUMP) then
          call IOTA  (WH, N, TITLE, 'Integrated WH'//LABEL)
        end if
      end if
C
      if(DUMP) then
        call MASHED  ('CRONOS')
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'CRONOS')
C     !END
      call BYE ('CRONOS')
C
      return
      end
