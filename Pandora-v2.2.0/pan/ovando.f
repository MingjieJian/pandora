      subroutine OVANDO
     $(CEQHH,HND,RSH,RNH1,N)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Computes RNH1, the number density of the groud state of neutral
C     Hydrogen, as a fraction of total Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, BRK, CEQHH, CRIT, EIGHT, HND, OMR, ONE,
     $       RNH1, RSH, RT, TWO, X, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external HI, BYE
C
C               CEQHH(N), HND(N), RSH(N), RNH1(N)
      dimension CEQHH(*), HND(*), RSH(*), RNH1(*)
C
      data A1,A2,A3,A4 /2.5D-1, 1.25D-1, 7.8125D-2, 5.46875D-2/
      data CRIT /1.D-3/
C
      call HI ('OVANDO')
C     !BEG
      do 100 I = 1,N
        OMR = ONE-RSH(I)
        if(OMR.ne.ZERO) then
C
          X = EIGHT*CEQHH(I)*HND(I)*OMR
          if(X.ge.CRIT) then
            RT  = sqrt(ONE+X)
            BRK = TWO*((RT-ONE)/X)
          else
            BRK = ONE-(A1-(A2-(A3-A4*X)*X)*X)*X
          end if
C
          RNH1(I) = OMR*BRK
C
        else
          RNH1(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('OVANDO')
C
      return
      end
