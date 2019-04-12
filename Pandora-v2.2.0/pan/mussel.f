      subroutine MUSSEL
     $(I,N,NL,CQUI,GMI,GVL,KDGV,BRJ,XDEN)
C
C     Rudolf Loeser, 1988 Jan 05
C---- Computes a denominator, for "b from b-ratios" calculation.
C     (This is version 2 of MUSSEL.)
C     !DASH
      save
C     !DASH
      real*8 BRJ, CQUI, GMI, GVL, TERM, XDEN, ZERO
      integer I, J, KDGV, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               CQUI(N,NSL), GMI(N,NSL), GVL(N,NL), BRJ(N,NL)
      dimension CQUI(N,*),   GMI(N,*),   GVL(N,*),  BRJ(N,*)
C
      call HI ('MUSSEL')
C     !BEG
      TERM = ZERO
      if(KDGV.eq.1) then
        TERM = GMI(I,J)*GVL(I,J)
      end if
C
      XDEN = ZERO
      do 100 J = 1,NL
        XDEN = XDEN+(CQUI(I,J)+TERM)*BRJ(I,J)
  100 continue
C     !END
      call BYE ('MUSSEL')
C
      return
      end
