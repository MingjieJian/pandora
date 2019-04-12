      subroutine NUMBLE
     $(IS,NZ,KK,XLB,SLY,SP,XJIK)
C
C     Rudolf Loeser, 2002 Apr 25
C---- Computes the lower part of XJIK, for I .gt. NE.
C
C     (Note: NZ equals N; NE (i.e. "n to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SLY, SP, XJIK, XLB
      integer I, IS, K, KK, NZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               SLY(NZ,KKX), XLB(NZ,KKX), SP(NZ,KKX), XJIK(NZ,KKX)
      dimension SLY(NZ,*),   XLB(NZ,*),   SP(NZ,*),   XJIK(NZ,*)
C
      call HI ('NUMBLE')
C     !BEG
      do 101 K = 1,KK
        do 100 I = IS,NZ
          XJIK(I,K) = (ONE-XLB(I,K))*SLY(I,K)+XLB(I,K)*SP(I,K)
  100   continue
  101 continue
C     !END
      call BYE ('NUMBLE')
C
      return
      end
