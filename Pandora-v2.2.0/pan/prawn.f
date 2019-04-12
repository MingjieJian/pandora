      subroutine PRAWN
     $(X,IX,W,IW,XLB1,JPROM,JTRANS,XKL,XKT,STAU)
C
C     Rudolf Loeser, 1998 Jul 29
C---- Sets up line-center TAU, etc., for MINUET.
C     !DASH
      save
C     !DASH
      real*8 STAU, W, X, XKL, XKT, XLB1
      integer IW, IX, JPROM, JTRANS
C     !DASH
      external WREN, SCALLOP, GASCON, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), STAU(N,NT), XKL(N), XKT(N)
      dimension XLB1(*),      STAU(*),    XKL(*), XKT(*)
C
      call HI ('PRAWN')
C     !BEG
C---- Compute and print line-center TAU
      call WREN    (X, IX, W, IW, XLB1, JPROM, XKL, XKT)
C
C---- Save TAU for later "fancy" printing
      call SCALLOP (XLB1, STAU)
C
C---- Save for "artificial" TAU
      call GASCON  (X, XLB1, JTRANS)
C     !END
      call BYE ('PRAWN')
C
      return
      end
