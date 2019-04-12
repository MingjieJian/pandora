      subroutine HANIBAL
     $(X,W,IW,XLB1,XLB2,XLM,DUMP,XCBL,Y,ILFLX,MPROM,MOVING,SPHERE,
     $ N,LG,MRR,XJNU,SOURCE,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1982 Jan 29
C---- Computes Jnu when opacity is angle-dependent.
C     !DASH
      save
C     !DASH
      real*8 SOURCE, W, X, XCBL, XJNU, XLB1, XLB2, XLM, Y
      integer ILFLX, IMG, ITS, IW, LAG, LG, MPROM, MRR, N
      logical DUMP, MOVING, SPHERE
C     !DASH
      external SAMOS, SALAMIS, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               SOURCE(N), XLB1(Li1len), XLB2(Li2len), IMG(N), XJNU(N),
      dimension SOURCE(*), XLB1(*),      XLB2(*),      IMG(*), XJNU(*),
C
C               XCBL(Miklen)
     $          XCBL(*)
C
      call HI ('HANIBAL')
C     !BEG
      if(SPHERE) then
C----   Spherical case
        call SAMOS   (X, W, IW, XLB1, XLB2, XLM, DUMP, XCBL, XJNU,
     $                SOURCE, ITS, LAG, Y, MOVING, ILFLX, MPROM, N,
     $                MRR, IMG)
      else
C----   Plane-parallel case
        call SALAMIS (X, W, IW, XLB1, XLB2, XLM, DUMP, XCBL, XJNU,
     $                SOURCE, ITS, LAG, Y, MOVING, ILFLX, MPROM, N,
     $                LG,  IMG)
      end if
C     !END
      call BYE ('HANIBAL')
C
      return
      end
