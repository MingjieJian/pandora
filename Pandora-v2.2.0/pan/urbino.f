      subroutine URBINO
     $(N,J,XNE,SA,GMI,ESG)
C
C     Rudolf Loeser, 2001 Dec 11
C---- Computes ESG = NE * SA * GM, for level J.
C     !DASH
      save
C     !DASH
      real*8 ESG, GMI, SA, XNE
      integer J, N
C     !DASH
      external ARRMUL, HI, BYE
C
C               XNE(N), SA(N), GMI(N,NSL), ESG(N,NSL)
      dimension XNE(*), SA(*), GMI(N,*),   ESG(N,*)
C
      call HI ('URBINO')
C     !BEG
      call ARRMUL (XNE, SA, ESG(1,J), N)
      call ARRMUL (ESG(1,J), GMI(1,J), ESG(1,J), N)
C     !END
      call BYE ('URBINO')
C
      return
      end
