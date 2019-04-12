      subroutine TANSIE
     $(X,IX,W,IW,XJBR,BDJ,IMG)
C
C     Rudolf Loeser, 1977 Dec 29
C---- Calculates B-ratios from Jbar, for TULIP.
C     !DASH
      save
C     !DASH
      real*8 BDJ, W, X, XJBR, dummy
      integer IMG, IW, IX, JL, JU, KBDSE, KRJ, MO
      character LEGEND*33
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MALDEN, FLIP, GIGGLE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XJBR(N,NT), BDJ(N,NL), IMG(N)
      dimension XJBR(*),    BDJ(*),    IMG(*)
C
      data KRJ,KBDSE,JU,JL /2, 1, 0, 0/
C
      call HI ('TANSIE')
C     !BEG
      call MALDEN (KRJ, 0, LEGEND)
      call GIGGLE (X, IX, W, dummy, XJBR, KRJ, KBDSE, JU, JL, LEGEND,
     $             MO)
      call FLIP   (X, IX, W, IW, KRJ, dummy, dummy, XJBR, BDJ, 0, 0,
     $             LEGEND, dummy, IMG)
C     !END
      call BYE ('TANSIE')
C
      return
      end
