      subroutine FERN
     $(XNUK,TNU,N,KK,XK,EXT,XINK,FINK,AK,Z,GK,V,F1,IQINC,NCR,XLCR,XICR,
     $ XL,GL,VL,U,EMUL,BDI,NL,KOLEV,TNUL,RNDT,DNRT,DNRTC,KOOL,XNU,CNXP,
     $ CP,IDNRT)
C
C     Rudolf Loeser, 1975 Jun 11
C---- Computes RNDT, DNRT and DNRTC, for KNOT.
C     !DASH
      save
C     !DASH
      real*8 AK, BDI, CNXP, CP, DNRT, DNRTC, EMUL, EXT, F1, FINK, GK,
     $       GL, RNDT, TNU, TNUL, U, V, VL, XICR, XINK, XK, XL, XLCR,
     $       XNU, XNUK, Z
      integer IDNRT, IQINC, KK, KOLEV, N, NCR, NL
      logical KOOL
C     !DASH
      external SHIVER, MIMOSA, ZERO1, HI, BYE
C
C               TNU(N,KKX), XK(KKX), Z(N), XINK(INK), FINK(INK), F1(N),
      dimension TNU(*),     XK(*),   Z(*), XINK(*),   FINK(*),   F1(*),
C
C               CP(NSL), V(N,KKX), XLCR(NCR), TNUL(N,NCR), CNXP(N,KKX),
     $          CP(*),   V(*),     XLCR(*),   TNUL(*),     CNXP(*),
C
C               XICR(NCR), XL(NCR), GL(NCR), VL(N,NCR), U(N), XNU(NSL),
     $          XICR(*),   XL(*),   GL(*),   VL(*),     U(*), XNU(*),
C
C               BDI(N,NL), RNDT(N), DNRT(N), DNRTC(N), AK(KKX), EXT(N),
     $          BDI(*),    RNDT(*), DNRT(*), DNRTC(*), AK(*),   EXT(*),
C
C               GK(KKX), EMUL(N,NCR)
     $          GK(*),   EMUL(*)
C
      call HI ('FERN')
C     !BEG
C---- Normal case - no incident radiation
      call ZERO1    (RNDT,  N)
      call ZERO1    (DNRT,  N)
      call ZERO1    (DNRTC, N)
C
      if(NCR.gt.0) then
C----   Special case - incident coronal radiation
        call SHIVER (KK, XK, GK, XNUK, NCR, XLCR, XICR, XL, GL, VL, U,
     $               EMUL, BDI, N, NL, KOLEV, TNUL, F1, RNDT, DNRT,
     $               KOOL, DNRTC, XNU, CP(KOLEV), EXT)
C
      else if(IQINC.gt.0) then
C----   General case of incident radiation
        call MIMOSA (KK, XK, AK, GK, N, Z, TNU, V, CNXP, F1, XINK,
     $               FINK, EXT, RNDT, DNRT, DNRTC, CP(KOLEV), KOOL,
     $               IDNRT)
      end if
C     !END
      call BYE ('FERN')
C
      return
      end
