      subroutine BUGBANE
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for "additional" continuum calculations.
C     (This is version 2 of BUGBANE.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IW, IWS, IX, JJWAV, JJYWA, JN, JOPAC, JOPAT, JSTCN, KADD,
     $        MUX, NSH, NWV, jummy1, jummy2
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(17),NWV)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 65),JJWAV)
      equivalence (IZOQ( 78),JJYWA)
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
C     !DASH
      external AVENS, KILO, MORTAIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C     !EJECT
C
      call HI ('BUGBANE')
C     !BEG
      call AVENS     (JSTCN, KADD, jummy1, jummy2)
C
      DOIT = (JSTCN.le.0).or.(KADD.eq.1).and.(NWV.gt.0)
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'BUGBANE')
C
        call KILO    (X, XCBL, X(JJWAV), X(JJYWA), IW(JOPAC),
     $                IW(JOPAT), SWAVE, SLTIT, NSH)
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'BUGBANE')
      end if
C     !END
      call BYE ('BUGBANE')
C
      return
      end
