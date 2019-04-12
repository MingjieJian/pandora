      subroutine KLAMATH
     $(X,W,IW,OPAC,Y,MOVING,WN,WH,ILFLX,IMG,TITLE,KODE)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Supervises weight matrices calculation.
C     !DASH
      save
C     !DASH
      real*8 OPAC, W, WH, WN, X, Y
      integer IFOP, ILFLX, IMG, IN, IQTOP, IS, ITDSK, ITSHL, IW, JJXDK,
     $        JJXSH, KD, KODE, KS, MOX
      logical MOVING, TOPT
      character TITLE*100
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 40),JJXSH)
      equivalence (IZOQ(132),JJXDK)
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
      equivalence (IQQ(156),IQTOP)
C     !DASH
C     !EJECT
      external GREBE, DAKINI, DASUNI, MITHRAS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               OPAC(N), WN(N,N), WH(N,N), IMG(N)
      dimension OPAC(*), WN(*),   WH(*),   IMG(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ITSHL ),(IN( 2),ITDSK ),(IN( 3),IFOP  )
C
      call HI ('KLAMATH')
C     !BEG
C     (Get, and allocate, W allotment)
      call GREBE     (IN,IS,MOX,'KLAMATH')
C
      TOPT = IQTOP.gt.0
C---- Compute Shell ray optical depths
      call DAKINI    (OPAC,W(IFOP),X(JJXSH),TOPT,W(ITSHL),KS,IMG,W)
C---- Compute Disk ray optical depths
      call DASUNI    (OPAC        ,X(JJXDK),TOPT,W(ITDSK),KD,IMG,W)
C---- Set error flag
      KODE = KS*KD
      if(KODE.eq.1) then
C----   Compute matrices
        call MITHRAS (X,W,IW,W(ITSHL),W(ITDSK),Y,MOVING,WN,WH,ILFLX,
     $                TITLE)
      end if
C
C     (Give back W allotment)
      call WGIVE     (W,'KLAMATH')
C     !END
      call BYE ('KLAMATH')
C
      return
      end
