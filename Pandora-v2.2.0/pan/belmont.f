      subroutine BELMONT
     $(X,IX,KRJ)
C
C     Rudolf Loeser, 1996 Apr 04
C---- Sets up control switch for statistical equilibrium calculations.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IX, JJYBR, KRJ
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 13),JJYBR)
C     !DASH
      external WAVERLY, HI, BYE
C
      dimension X(*), IX(*)
C
C
      call HI ('BELMONT')
C     !BEG
      call WAVERLY (IX, X(JJYBR), KRJ)
C     !END
      call BYE ('BELMONT')
C
      return
      end
