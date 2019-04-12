      subroutine IGUANA
     $(A,JU,JL,IB,IE,TER,J,NTE,ARR,K)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Selects Collisional Transitions data to be printed, for LIZARD.
C     (This is version 2 of IGUANA.)
C     !DASH
      save
C     !DASH
      real*8 A, ARR, TER, ZERO
      integer I, IB, IE, IN, J, JL, JU, K, NTE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, INDXUL, HI, BYE
C
C               A(NTE,MUL), TER(NTE), JU(MUL), JL(MUL), ARR(8)
      dimension A(NTE,*),   TER(*),   JU(*),   JL(*),   ARR(*)
C
      call HI ('IGUANA')
C     !BEG
      K = 1
      if(NTE.le.0) then
        ARR(K) = ZERO
      else
        ARR(K) = TER(J)
      end if
C
      call ZERO1    (ARR(2), 7)
C
      do 100 I = IB,IE
        K = K+1
        call INDXUL (JU(I), JL(I), IN)
        ARR(K) = A(J,IN)
  100 continue
C     !END
      call BYE ('IGUANA')
C
      return
      end
