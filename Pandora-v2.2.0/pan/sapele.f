      subroutine SAPELE
     $(X,W,IW,TNU,OPAC,Z,EMUX,XK,KK,AK,Y,GK,V,XLB,D,F1,P,NE,NZ,IMG,
     $ WNSAV,IXUSE,DUMP,DMPW)
C
C     Rudolf Loeser, 1968 Aug 06
C---- Controls the calculation of P, for TAURUS.
C
C     (Note: NZ equals N; NE (i.e. "N to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 AK, D, EMUX, F1, GK, OPAC, P, TNU, V, W, WNSAV, X, XK, XLB,
     $       Y, Z
      integer IEXT, IGP, IMG, IN, IS, IW, IXUSE, KK, MOX, NE, NZ
      logical DMPW, DUMP
C     !DASH
      external IPIL, DURIAN, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TNU(NZ,KKX), P(NE,NE), IXUSE(KKX), IMG(N), XLB(NZ,KKX),
      dimension TNU(*),      P(*),     IXUSE(*),   IMG(*), XLB(*),
C
C               WNSAV(NZ,NZ,KKX), V(NZ,KKX), XK(KKX), AK(KKX), GK(KKX),
     $          WNSAV(*),         V(*),      XK(*),   AK(*),   GK(*),
C
C               D(NE), Z(NZ), EMUX(NZ,KKX), OPAC(NZ,KKX), F1(NE)
     $          D(*),  Z(*),  EMUX(*),      OPAC(*),      F1(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IGP   ),(IN( 2),IEXT  )
C
      call HI ('SAPELE')
C     !BEG
C     (Get, and allocate, W allotment)
      call IPIL   (IN, IS, MOX, 'SAPELE', NE)
C
      call DURIAN (X, W, IW, TNU, OPAC, Z, EMUX, XK, KK, AK, Y, GK, V,
     $             XLB, D, F1, P, NE, NZ, WNSAV, IXUSE, W(IGP), IMG,
     $             W(IEXT), DUMP, DMPW)
C
C     (Give back W allotment)
      call WGIVE  (W, 'SAPELE')
C     !END
      call BYE ('SAPELE')
C
      return
      end
