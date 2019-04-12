      subroutine GRUMP
     $(X,IX,W,IW,XLB1,XCBL,JPROM,JTRANS,MSFT,XKL,XKT,STAU)
C
C     Rudolf Loeser, 2006 Feb 24
C---- Sets up intermediates for LSF calculation.
C     !DASH
      save
C     !DASH
      real*8 STAU, W, X, XCBL, XKL, XKT, XLB1
      integer IW, IX, JPROM, JTRANS, MSFT
C     !DASH
      external PLOW, PRAWN, AMADEUS, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XCBL(Miklen), STAU(N,NT), XKT(N), XKL(N)
      dimension XLB1(*),      XCBL(*),      STAU(*),    XKT(*), XKL(*)
C
      call HI ('GRUMP')
C     !BEG
C---- Get line-center background data: CSF, COP, XJNU
      call PLOW    (XCBL, XLB1)
C
C---- Get various line-center TAUs
      call PRAWN   (X, IX, W, IW, XLB1, JPROM, JTRANS, XKL, XKT, STAU)
C
C---- Reset LSF method = Sobolev (if necessary)
      call AMADEUS (XLB1, MSFT)
C     !END
      call BYE ('GRUMP')
C
      return
      end
