      subroutine FALCON
     $(N,Y,T,F,C)
C
C     Rudolf Loeser, 1971 Sep 20
C---- Computes coefficients for the quadratic representation of F(T).
C     !DASH
      save
C     !DASH
      real*8 C, D, F, S, SUM, T, TAU, Y, ZERO
      integer I, J, JJ, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SEGMENT, DIVIDE, HI, BYE
C
C               T(N), F(N), C(N)
      dimension T(*), F(*), C(*)
C
      call HI ('FALCON')
C     !BEG
      D = T(N)-T(N-1)
      call DIVIDE      ((F(N)-F(N-1))            ,D,C(N))
      call DIVIDE      ((T(N)*F(N-1)-T(N-1)*F(N)),D,C(1))
C
      I = N
      do 101 J = 1,(N-2)
        TAU = T(I-2)
        SUM = ZERO
        do 100 JJ = I,N
          call SEGMENT (JJ,N,Y,TAU,T(JJ),S)
          SUM = SUM+C(JJ)*S
  100   continue
C
        I = I-1
        call SEGMENT   (I,N,Y,TAU,T(I),S)
        call DIVIDE    ((F(I-1)-C(1)-SUM)        ,S,C(I))
  101 continue
C     !END
      call BYE ('FALCON')
C
      return
      end
