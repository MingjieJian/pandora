      subroutine DUGOUT
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Sets up data blocks for CO continuum calculations.
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IJIND, IKIND, IMIND, IN, ININD, IS, IW, IWAVE, IWS, IX,
     $        JJXCA, JJXCB, JN, JOPAC, JOPAT, JSTCN, KCO, MOX, MUX, NCB,
     $        NCL, NSH, jummy1, jummy2
      logical DOIT, SWTA, SWTB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(55),NCB)
      equivalence (JZQ(51),NCL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(221),JJXCA)
      equivalence (IZOQ(222),JJXCB)
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
      external JIBBER, AVENS, UMIAK, MERANIA, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Length), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IWAVE )
C
      dimension JN(6)
      equivalence
     $(JN( 1),IJIND ),(JN( 2),IKIND ),(JN( 3),ININD ),(JN( 4),IMIND ),
     $(JN( 5),JOPAC ),(JN( 6),JOPAT )
C     !EJECT
C
      call HI ('DUGOUT')
C     !BEG
      call AVENS     (JSTCN, jummy1, jummy2, KCO)
C
      SWTA = (JSTCN.le.0).or.(KCO.eq.1)
      SWTB = (NCB.gt.0).and.(NCL.gt.0)
      DOIT = SWTA.and.SWTB
C
      if(DOIT) then
C       (Get, and allocate, W & IW allotments)
        call JIBBER  (IN, IS,  MOX, 'DUGOUT')
        call MERANIA (JN, IWS, MUX, 'DUGOUT')
C
        call UMIAK   (X, W, IW, XCBL, X(JJXCA), X(JJXCB), W(IWAVE),
     $                IW(IJIND), IW(IKIND), IW(ININD), IW(IMIND),
     $                IW(JOPAC), IW(JOPAT), SWAVE, SLTIT, NSH)
C
C       (Give back W & IW allotments)
        call WGIVE   (W,  'DUGOUT')
        call IGIVE   (IW, 'DUGOUT')
      end if
C     !END
      call BYE ('DUGOUT')
C
      return
      end
