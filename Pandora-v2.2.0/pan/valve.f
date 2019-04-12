      subroutine VALVE
     $(XNU,ALPHA)
C
C     Rudolf Loeser, 1982 Feb 17
C---- Computes ALPHA(nu); (XNU is in frequency units).
C     (This is version 2 of VALVE.)
C     !DASH
      save
C     !DASH
      real*8 ALPHA, CON7, XNU
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('VALVE')
C     !BEG
      call RIGEL (7, CON7)
      ALPHA = CON7*(XNU**3)
C     !END
      call BYE ('VALVE')
C
      return
      end
