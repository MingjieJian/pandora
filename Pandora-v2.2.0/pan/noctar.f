      subroutine NOCTAR
     $(N,NL,XNK,XNKPR,XND,XNDPR,BDI,BDIPR)
C
C     Rudolf Loeser, 2003 Apr 29
C---- Saves previous results, for GORSE.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIPR, XND, XNDPR, XNK, XNKPR
      integer N, NL, NNL
C     !DASH
      external MOVE1, HI, BYE
C
C               XNDPR(N,NL), BDIPR(N,NL), XND(N,NL), BDI(N,NL), XNK(N),
      dimension XNDPR(*),    BDIPR(*),    XND(*),    BDI(*),    XNK(*),
C
C               XNKPR(N)
     $          XNKPR(*)
C
      call HI ('NOCTAR')
C     !BEG
      NNL = N*NL
      call MOVE1 (XNK, N  , XNKPR)
      call MOVE1 (XND, NNL, XNDPR)
      call MOVE1 (BDI, NNL, BDIPR)
C     !END
      call BYE ('NOCTAR')
C
      return
      end
