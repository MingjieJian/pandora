      subroutine ORPINE
     $(X,IX,W,IW,XCBL)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for line core continuum calculations.
C     (This is version 2 of ORPINE.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL
      integer IW, IWS, IX, JJOML, JJXNU, JJYCO, JN, JOPAC, JOPAT, JSTCN,
     $        MUX, NOION
      logical DOIT
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ(  3),JJOML)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
C     !DASH
      external GOLF, MORTAIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C     !EJECT
C
      call HI ('ORPINE')
C     !BEG
      DOIT = (NOION.le.0).and.(JSTCN.le.0)
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'ORPINE')
C
        call GOLF    (X, XCBL, X(JJXNU), X(JJYCO), X(JJOML), IW(JOPAC),
     $                IW(JOPAT))
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'ORPINE')
      end if
C     !END
      call BYE ('ORPINE')
C
      return
      end
