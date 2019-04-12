      subroutine ZORNIG
     $(N,ZME,RZM,AEL,ZHEL,ZRN)
C
C     Rudolf Loeser, 2002 Jan 25
C     Revised RL/SGK Apr  9 2014 
C---- Computes ZRN, the number density of ions other than protons
C     !DASH
      save
C     !DASH
      real*8 AEL, RZM, ZHEL, ZME, ZRN
      integer I, N
C     !DASH
      external HI, BYE
C
C               ZHEL(N), ZME(N), RZM(N), AEL(N), ZRN(N)
      dimension ZHEL(*), ZME(*), RZM(*), AEL(*), ZRN(*)
C
      call HI ('ZORNIG')
C     !BEG
      do 100 I = 1,N
        ZRN(I) =  ZME(I)*RZM(I) + AEL(I) + ZHEL(I)
  100 continue
C     !END
      call BYE ('ZORNIG')
C
      return
      end
