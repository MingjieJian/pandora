      subroutine ANGRY
     $(X,ZME,ZHEL,ZRN)
C
C     Rudolf Loeser, 2002 Jan 25
C     Revised RL/SGK Apr  9 2014 
C---- Calculates ZRN: number density of ions other than protons.
C     !DASH
      save
C     !DASH
      real*8 X, ZME, ZHEL, ZRN
      integer JJAEL, JJRZM, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 80),JJAEL)
C     !DASH
      external ZORNIG, HI, BYE
C
      dimension X(*)
C
C               ZHEL(N), ZME(N), ZRN(N)
      dimension ZHEL(*), ZME(*), ZRN(*)
C
      call HI ('ANGRY')
C     !BEG
      call ZORNIG (N, ZME, X(JJRZM), X(JJAEL), ZHEL, ZRN)
C     !END
      call BYE ('ANGRY')
C
      return
      end
