      subroutine HUFF
     $(INDX,XLM,N,NOPAC,XNE,HNK,TE,CONT)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes a set of Hydrogen free-free opacity values.
C     (This is version 2 of HUFF.)
C     !DASH
      save
C     !DASH
      real*8 A, CONT, F, G, HNK, RT, ST, TE, X, XLM, XNE, XP, dummy
      integer INDX, J, N, NOPAC
C     !DASH
      external DIVIDE, HUNK, QEXP1, GAUNT, HI, BYE
C
C               XNE(N), HNK(N), TE(N), CONT(Nopac,N)
      dimension XNE(*), HNK(*), TE(*), CONT(NOPAC,*)
C
      data A /1.37D-47/
C
      call HI ('HUFF')
C     !BEG
      XP = XLM**3
      do 100 J = 1,N
        RT = sqrt(TE(J))
        call DIVIDE ((A*XNE(J)*HNK(J)), RT, F)
        call HUNK   (TE(J), XLM, 2, X)
        call QEXP1  (X, dummy, 2, ST)
        call GAUNT  (TE(J), XLM, G)
        CONT(INDX,J) = F*XP*ST*G
  100 continue
C     !END
      call BYE ('HUFF')
C
      return
      end
