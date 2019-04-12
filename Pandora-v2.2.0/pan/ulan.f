      subroutine ULAN
     $(X,N,ABDEL,ABDION)
C
C     Rudolf Loeser, 2000 Jan 04
C---- Computes element abundance w.r.t. Hydrogen, and
C     absolute ion abundance.
C     !DASH
      save
C     !DASH
      real*8 ABD, ABDEL, ABDION, X
      integer JJHND, JJRAB, N
      character QELSM*8
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 11),JJHND)
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
      equivalence (QZQ(  2),QELSM)
      equivalence (RZQ(  6),ABD  )
C     !DASH
      external LUNA, ARRMUL, HI, BYE
C
      dimension X(*)
C
C               ABDEL(N), ABDION(N)
      dimension ABDEL(*), ABDION(*)
C
      call HI ('ULAN')
C     !BEG
      call LUNA   (X,QELSM,ABD,ABDEL)
      call ARRMUL (ABDEL ,X(JJRAB),ABDION,N)
      call ARRMUL (ABDION,X(JJHND),ABDION,N)
C     !END
      call BYE ('ULAN')
C
      return
      end
