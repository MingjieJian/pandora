      subroutine LVOIGT
     $(X,A,VOIGT)
C     Rudolf Loeser, 1978 Dec 14
C---- Drives MODRV, which is a version of RVOIGT.
C     CRT=1.E-5 gives limited accuracy from MODRV,
C     usually about 6 figures, but occasionally slightly less than 3.
C     This results in a time saving over RVOIGT of about 1/2.
C     !DASH
      save
C     !DASH
      real*8 A, VOIGT, X
      real*4 AA, CRT, V, XX
C     !DASH
      external MODRV
C
      data CRT /1.E-5/
C
C     !BEG
      XX = X
      AA = A
      call MODRV (XX,AA,CRT,V)
      VOIGT = V
C     !END
C
      return
      end
