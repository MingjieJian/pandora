      subroutine AIRE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 23
C---- H. S. E. - Hydrostatic Equilibrium calculation.
C     (This is version 2 of AIRE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, FIZZ, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /15/
C
      call HI ('AIRE')
C     !BEG
      call LOGIN  (NPROG)
      call FIZZ   (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('AIRE')
C
      return
      end
