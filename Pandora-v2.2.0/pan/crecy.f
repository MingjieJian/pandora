      subroutine CRECY
     $(MRP,RNU,RCP,YW,ITAU,N,RJ,UJ,KOOL, FA,RL)
C
C     Rudolf Loeser, 1974 Jun 14
C---- Computes an RL integral.
C     !DASH
      save
C     !DASH
      real*8 FA, FL, FR, HALF, RCP, RJ, RL, RNU, UJ, YW, ZERO
      integer ITAU, J, MRP, N
      logical KOOL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external  KATUN, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), YW(N,MRX+1), FA(N,MRX+1), UJ(N)
      dimension RNU(*),     RCP(*),     YW(N,*),     FA(N,*),     UJ(*)
C
      call HI ('CRECY')
C     !BEG
      RL = ZERO
C
      if(MRP.le.1) then
        FA(ITAU,1) = ZERO
      else
C
        call KATUN   (RCP(1), RNU(1), UJ(ITAU), RJ, YW(ITAU,1), KOOL,
     $                FA(ITAU,1), FR)
C
        do 100 J = 2,MRP
          FL = FR
          call KATUN (RCP(J), RNU(J), UJ(ITAU), RJ, YW(ITAU,J), KOOL,
     $                FA(ITAU,J), FR)
C
          RL = RL+HALF*((FL+FR)*(RNU(J)-RNU(J-1)))
  100   continue
      end if
C     !END
      call BYE ('CRECY')
C
      return
      end
