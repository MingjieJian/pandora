      subroutine MYRTLE
     $(XI,K,Y,A,F,G,W,IW,KODE)
C
C     Rudolf Loeser, 1968 Mar 29
C---- Computes A, the weights for the Line Source Function
C     frequency summations.
C     Returns with KODE=1 if all seems ok, =0 if not.
C     (See also CHERRY.)
C     !DASH
      save
C     !DASH
      real*8 A, F, G, HALF, ONE, T, THIRD, W, XI, Y, ZERO
      integer IW, J, K, KODE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MADRONE, ZERO1, SUMPROD, HI, BYE
C
      dimension W(*), IW(*)
C
C               XI(K), A(K), F(K,K), G(K)
      dimension XI(*), A(*), F(K,*), G(*)
C
      call HI ('MYRTLE')
C     !BEG
      call ZERO1       (A, K)
C
C---- Compute the inverse of the matrix of modified F-functions
      call MADRONE     (XI, K, Y, F, W, IW, KODE)
C
      if(KODE.eq.1) then
C----   Compute the vector G
        G(1) = XI(K)
C
        T = HALF*(ONE-THIRD*Y)
        do 100 J = 2,K
          G(J) = XI(J)*T
  100   continue
C
C----   and now multiply to form A
        do 101 J = 1,K
          call SUMPROD (A(J), G, 1, F(1,J), 1, K)
  101   continue
      end if
C     !END
      call BYE ('MYRTLE')
C
      return
      end
