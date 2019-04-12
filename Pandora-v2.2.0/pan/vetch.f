      subroutine VETCH
     $(X,IX,W,IW,RHOIJ,RHOO,YBAR,BDR,IMG,IFUDGE,KODE,FUDGE,ITER)
C
C     Rudolf Loeser, 1977 Dec 29
C---- Calculates b-ratios from RHO with JBAR, for TULIP.
C     !DASH
      save
C     !DASH
      real*8 BDR, FUDGE, RHOIJ, RHOO, W, X, YBAR
      integer IFUDGE, IMG, ITER, IW, IX, JL, JU, KBDSE, KODE, KRJ, MO
      character LEGEND*33
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MALDEN, GIGGLE, FLIP, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RHOIJ(N,NT), RHOO(N,NT), IMG(N), BDR(N,NL), YBAR(N,NT)
      dimension RHOIJ(*),    RHOO(*),    IMG(*), BDR(*),    YBAR(*)
C
      data KRJ,KBDSE,JU,JL /1, 1, 0, 0/
C
      call HI ('VETCH')
C     !BEG
      call MALDEN (KRJ, ITER, LEGEND)
      call GIGGLE (X, IX, W, RHOIJ, YBAR, KRJ, KBDSE, JU, JL, LEGEND,
     $             MO)
      call FLIP   (X, IX, W, IW, KRJ, RHOIJ, RHOO, YBAR, BDR, IFUDGE,
     $             KODE, LEGEND, FUDGE, IMG)
C     !END
      call BYE ('VETCH')
C
      return
      end
