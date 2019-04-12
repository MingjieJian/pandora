      subroutine KIBEL
     $(XO,X,N,CHK)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Computes CHK, for BILK.
C     CHK(i) is the largest off-diagonal element of row i of XO x X.
C     !DASH
      save
C     !DASH
      real*8 BIG, CHK, SUM, X, XO, ZERO
      integer I, J, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
C               XO(N,N), X(N,N), CHK(N)
      dimension XO(N,*), X(N,*), CHK(*)
C
      call HI ('KIBEL')
C     !BEG
      do 102 I = 1,N
        BIG = ZERO
        do 101 J = 1,N
          if(I.ne.J) then
            SUM = ZERO
            do 100 K = 1,N
              SUM = SUM+XO(I,K)*X(K,J)
  100       continue
            if(abs(SUM).gt.abs(BIG)) then
              BIG = SUM
            end if
          end if
  101   continue
        CHK(I) = BIG
  102 continue
C     !END
      call BYE ('KIBEL')
C
      return
      end
