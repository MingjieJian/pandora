      subroutine FELIX
     $(N,LG,XA,WN,RR,XJNU,ITS,CI,SUM1,SUM2,XJNUO,CSFCRIT,MXKNT,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Attempts to compute XJNU iteratively, for angle-dependent
C     Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CI, CSFCRIT, RR, SUM1, SUM2, WN, XA, XJNU, XJNUO, dummy
      integer IMX, ITER, ITS, KNT, LG, MXKNT, N, jummy
      logical DUMP, SAME
C     !DASH
      external ZERO1, MOVE1, FOTOR, CONVERD, HI, BYE
C
C               XA(N,N,LG), SUM2(N), RR(N), XJNU(N), CI(N,LG), SUM1(N),
      dimension XA(*),      SUM2(*), RR(*), XJNU(*), CI(*),    SUM1(*),
C
C               XJNUO(N), WN(N,N,LG)
     $          XJNUO(*), WN(*)
C
      call HI ('FELIX')
C     !BEG
C---- Initialize
      call ZERO1     (XJNU, N)
      IMX = MXKNT+1
      KNT = -1
C
C---- Iterate
      do 100 ITER = 1,IMX
        KNT = KNT+1
C----   Move "current Jnu" into "previous Jnu" slot
        call MOVE1   (XJNU, N, XJNUO)
C----   Compute new "current Jnu"
        call FOTOR   (N, LG, CI, XA, WN, RR, XJNUO, SUM1, SUM2, XJNU,
     $                ITER, DUMP)
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
      call BYE ('FELIX')
C
      return
      end
