      subroutine FLECK
     $(UJI,RJ,X,Y,F)
C
C     Rudolf Loeser, 1984 May 02
C---- Computes function F, for KATUN.
C     !DASH
      save
C     !DASH
      real*8 EX, F, RJ, UJI, X, Y
C     !DASH
      external HI, BYE
C
      call HI ('FLECK')
C     !BEG
      EX = exp(-UJI*X)
      F  = (RJ*(X**3)+Y)*EX
C     !END
      call BYE ('FLECK')
C
      return
      end
