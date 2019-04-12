      subroutine NALDOR
     $(AIJ,AATIJ,JU,JL,IB,IE,NL,ARR,K)
C
C     Rudolf Loeser, 1999 Oct 27
C---- Sets up "A"-values to be printed, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 A, AATIJ, AIJ, ARR, ZERO
      integer I, IB, IE, JL, JU, K, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, HI, BYE
C
C               AATIJ(NL,NL), AIJ(NL,NL), JU(MUL), JL(MUL), ARR(8)
      dimension AATIJ(NL,*),  AIJ(NL,*),  JU(*),   JL(*),   ARR(*)
C
      call HI ('NALDOR')
C     !BEG
      call ZERO1 (ARR, 8)
      K = 1
      do 100 I = IB,IE
        K = K+1
        A = AIJ(JU(I),JL(I))
        if(A.gt.ZERO) then
          ARR(K) =  A
        else
          ARR(K) = -AATIJ(JU(I),JL(I))
        end if
  100 continue
C     !END
      call BYE ('NALDOR')
C
      return
      end
