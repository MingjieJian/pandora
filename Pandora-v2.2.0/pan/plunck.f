      subroutine PLUNCK
     $(XLM,T,B)
C
C     Rudolf Loeser, 1981 Jul 27.
C---- Computes a single value of the Planck function.
C     (This is version 2 of PLUNCK.)
C     !DASH
      save
C     !DASH
      real*8 B, CON, E, F, R, T, W, XLM
C     !DASH
      external HUNK, QEXP1, DIVIDE, RIGEL, HI, BYE
C
      call HI ('PLUNCK')
C     !BEG
      call HUNK   (T,XLM,2,W)
      call QEXP1  (W,E,0,F)
      call DIVIDE (E,F,R)
      call RIGEL  (6,CON)
C
      B = R*(CON/(XLM**3))
C     !END
      call BYE ('PLUNCK')
C
      return
      end
