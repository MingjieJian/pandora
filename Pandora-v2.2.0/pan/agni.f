      subroutine AGNI
     $(X,W,IW,GOODS,GOODF)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Controls computation of
C     COMMON Line Source Function frequency integration weights.
C     !DASH
      save
C     !DASH
      real*8 W, X, Y, YAFUL, YASYM
      integer IW, JJAF, JJAS, JJXIF, JJXIS, KF, KS
      logical GOODF, GOODS
      character LAB*10
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(36),KS )
      equivalence (JZQ( 4),KF )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ(121),JJXIF)
      equivalence (IZOQ(147),JJAS )
      equivalence (IZOQ(122),JJAF )
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
      equivalence (RZQ(  8),Y    )
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
      equivalence (REST( 6),YASYM)
      equivalence (REST( 7),YAFUL)
C     !DASH
C     !EJECT
      external COWRY, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      data LAB /'Standard A'/
C
      call HI ('AGNI')
C     !BEG
C---- For symmetric (half) profile
      call COWRY (X(JJXIS), KS, Y, X(JJAS), W, IW, LAB, YASYM, GOODS)
C---- For full profile
      call COWRY (X(JJXIF), KF, Y, X(JJAF), W, IW, LAB, YAFUL, GOODF)
C     !END
      call BYE ('AGNI')
C
      return
      end
