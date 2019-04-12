      subroutine COMO
     $(XLM,XNE,TE,HND,N,CQT,CQA,NCQ,CQM,PNH,CLM,ALB)
C
C     Rudolf Loeser, 1988 Dec 16
C---- Computes a set of scattering albedo values.
C     (This is version 2 of COMO.)
C     !DASH
      save
C     !DASH
      real*8 ALB, CLM, CQA, CQM, CQT, CQV, HND, PNH, RNH, TE, XLM, XNE
      integer I, N, NCQ
C     !DASH
      external LECCO, LYDDA, DIVIDE, SPOWER, HI, BYE
C
C               XNE(N), TE(N), CQT(NCQ), CQA(NCQ), ALB(N), HND(N)
      dimension XNE(*), TE(*), CQT(*),   CQA(*),   ALB(*), HND(*)
C
      call HI ('COMO')
C     !BEG
      do 100 I = 1,N
        call LYDDA  (CQT, CQA, NCQ, TE(I), CQM, CQV)
        call DIVIDE (HND(I), HND(N), RNH)
        call LECCO  (XNE(I), RNH, PNH, CLM, XLM, CQV, ALB(I))
  100 continue
C     !END
      call BYE ('COMO')
C
      return
      end
