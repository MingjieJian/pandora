      subroutine DUMBLE
     $(NE,XLB,SLY,SP,WN,XJIK)
C
C     Rudolf Loeser, 2002 Apr 25
C---- Computes the upper portion of XJIK, for I .le. NE.
C
C     (Note: NZ equals N; NE (i.e. "N to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SLY, SP, WN, XJ, XJIK, XLB
      integer I, J, NE
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
C               WN(NE,NE), SLY(NZ), XLB(NZ), SP(NZ), XJIK(NZ)
      dimension WN(NE,*),  SLY(*),  XLB(*),  SP(*),  XJIK(*)
C
      call HI ('DUMBLE')
C     !BEG
      do 101 I = 1,NE
        XJ = (ONE-XLB(I))*SLY(I)+XLB(I)*SP(I)
        do 100 J = 1,NE
          XJ = XJ+WN(I,J)*((ONE-XLB(J))*SLY(J)+XLB(J)*SP(J))
  100   continue
        XJIK(I) = XJ
  101 continue
C     !END
      call BYE ('DUMBLE')
C
      return
      end
