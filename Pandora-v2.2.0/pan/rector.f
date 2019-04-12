      subroutine RECTOR
     $(TE,THETA)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Computes a value of reciprocal temperature.
C     !DASH
      save
C     !DASH
      real*8 CON44, TE, THETA
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
      call HI ('RECTOR')
C     !BEG
      call RIGEL  (44,CON44)
      call DIVIDE (CON44,TE,THETA)
C     !END
      call BYE ('RECTOR')
C
      return
      end
