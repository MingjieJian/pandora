      subroutine ELIOT
     $(N,RZM,ZME,AEL,HND,ZMEL,ZMER)
C
C     Rudolf Loeser, 1980 Jul 29
C     Revised RL/SGK Apr 15 2014 
C---- Computes ZMEL, the number density of metal electrons.
C     (This is version 4 of ELIOT.)
C     !DASH
      save
C     !DASH
      real*8 AEL, HND, RZM, ZME, ZMEL, ZMER
      integer N
C     !DASH
      external ARRMUL, ARRADD, ARRDIV, HI, BYE
C
C               RZM(N), ZME(N), AEL(N), HND(N), ZMEL(N), ZMER(N)
      dimension RZM(*), ZME(*), AEL(*), HND(*), ZMEL(*), ZMER(*)
C
      call HI ('ELIOT')
C     !BEG
      call ARRMUL (RZM ,ZME,ZMEL,N)
      call ARRADD (ZMEL,AEL,ZMEL,N)
      call ARRDIV (ZME ,HND,ZMER,N)
C     !END
      call BYE ('ELIOT')
C
      return
      end
