      subroutine PASTA
     $(N,NSHL,CSHL,XASHL,WNSHL,MRR,CDSK,XADSK,WNDSK,RR,XJNU,ITS,SUM1,
     $ SUM2,XJNUO,CSFCRIT,MXKNT,DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Attempts to compute XJNU iteratively, for angle-dependent
C     Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSFCRIT, CSHL, RR, SUM1, SUM2, WNDSK, WNSHL, XADSK,
     $       XASHL, XJNU, XJNUO, dummy
      integer ITER, ITS, KNT, MRR, MXKNT, N, NSHL, jummy
      logical DUMP, SAME
C     !DASH
      external ZERO1, MOVE1, RUGILA, CONVERD, HI, BYE
C
C               XJNU(N), XJNUO(N), CSHL(N,NSHL), WNDSK(N,N,MRR), RR(N),
      dimension XJNU(*), XJNUO(*), CSHL(*),      WNDSK(*),       RR(*),
C
C               XASHL(N,N,NSHL), WNSHL(N,N,NSHL), CDSK(N,MRR), SUM2(N),
     $          XASHL(*),        WNSHL(*),        CDSK(*),     SUM2(*),
C
C               XADSK(N,N,MRR), SUM1(N)
     $          XADSK(*),       SUM1(*)
C
C
      call HI ('PASTA')
C     !BEG
C---- Initialize
      call ZERO1     (XJNU, N)
C
C---- Iterate
      KNT = -1
      do 100 ITER = 1,(MXKNT+1)
        KNT = KNT+1
C----   Move "current Jnu" into "previous Jnu" slot
        call MOVE1   (XJNU, N, XJNUO)
C----   Compute new "current Jnu"
        call RUGILA  (N, NSHL, CSHL, XASHL, WNSHL, MRR, CDSK, XADSK,
     $                WNDSK, RR, XJNUO, SUM1, SUM2, XJNU, ITER, DUMP)
C----   Check for convergence
        call CONVERD (XJNU, 1, N, XJNUO, 1, N, CSFCRIT, dummy, jummy,
     $                SAME)
        if(SAME) goto 101
  100 continue
C
  101 continue
C---- Set iteration code
      ITS = KNT
      if(ITS.ge.MXKNT) then
        ITS = 99
      end if
C     !END
      call BYE ('PASTA')
C
      return
      end
