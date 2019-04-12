      subroutine DURAS
     $(MRP,RNU,RCP,YW,ITAU,N,KOOL, RK)
C
C     Rudolf Loeser, 1974 Jun 14
C---- Computes the RK integral.
C     !DASH
      save
C     !DASH
      real*8 FL, FR, HALF, RCP, RK, RNU, YW, ZERO
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
      external ELLIS, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), YW(N,MRX+1)
      dimension RNU(*),     RCP(*),     YW(N,*)
C
      call HI ('DURAS')
C     !BEG
      RK = ZERO
C
      if(MRP.gt.1) then
C
        call ELLIS   (RCP(1),RNU(1),YW(ITAU,1),KOOL, FR)
C
        do 100 J = 2,MRP
          FL = FR
          call ELLIS (RCP(J),RNU(J),YW(ITAU,J),KOOL, FR)
          RK = RK+HALF*((FL+FR)*(RNU(J)-RNU(J-1)))
  100   continue
      end if
C     !END
      call BYE ('DURAS')
C
      return
      end
