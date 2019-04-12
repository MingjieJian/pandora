      subroutine EXCEL
     $(KK,XK,GK,XNUK,NCR,XLCR,XL,GL)
C
C     Rudolf Loeser, 1975 Jun 11
C---- Computes XL and GL, for SHIVER.
C     !DASH
      save
C     !DASH
      real*8 CON16, GK, GL, T, XK, XL, XLCR, XNUK
      integer KK, NCR
C     !DASH
      external RIGEL, MOVE1, RECIPRO, CONMUL, FERE, HI, BYE
C
C               XLCR(NCR), XL(NCR), GL(NCR), XK(KKX), GK(KKX)
      dimension XLCR(*),   XL(*),   GL(*),   XK(*),   GK(*)
C
      call HI ('EXCEL')
C     !BEG
      call RIGEL   (16, CON16)
      T = CON16/XNUK
      call MOVE1   (XLCR, NCR, XL)
      call RECIPRO (XL, NCR)
      call CONMUL  (T, XL, NCR)
      call FERE    (XK, 1, GK, 1, KK, XL, 1, GL, 1, NCR, 1, 1)
C     !END
      call BYE ('EXCEL')
C
      return
      end
