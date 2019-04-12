      subroutine FULMAR
     $(TR1,RK1,TR2,RK2,TRFLI,TR,RK)
C
C     Rudolf Loeser, 1984 Nov 16
C---- Computes TR, for LUGGAGE.
C     !DASH
      save
C     !DASH
      real*8 DR, HALF, RAT, RK, RK1, RK2, TR, TR1, TR2, TRFLI, ZERO
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
      external  HI, BYE
      intrinsic min, max
C
      call HI ('FULMAR')
C     !BEG
      DR = RK2-RK1
      if(DR.eq.ZERO) then
        RAT = HALF
      else
        RAT = (TR2-TR1)/DR
      end if
      TR = TR1+(RK-RK1)*RAT
      TR = max(TR,(TR1/TRFLI))
      TR = min(TR,(TR2*TRFLI))
C     !END
      call BYE ('FULMAR')
C
      return
      end
