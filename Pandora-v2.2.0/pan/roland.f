      subroutine ROLAND
     $(A,JU,JL,IB,IE,NL,AIJ,ARR,K)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Selects "AIJ.ne.0" transitions data to be printed, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 A, AIJ, ARR, ZERO
      integer I, IB, IE, IUL, JL, JU, K, NL
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
C               A(MUL), AIJ(NL,NL), JU(MUL), JL(MUL), ARR(8)
      dimension A(*),   AIJ(NL,*),  JU(*),   JL(*),   ARR(*)
C
      call HI ('ROLAND')
C     !BEG
      call ZERO1      (ARR,8)
      K = 1
      do 100 I = IB,IE
        K = K+1
        if(AIJ(JU(I),JL(I)).ne.ZERO) then
          call INDXUL (JU(I),JL(I),IUL)
          ARR(K) = A(IUL)
        end if
  100 continue
C     !END
      call BYE ('ROLAND')
C
      return
      end
