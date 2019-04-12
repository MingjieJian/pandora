      subroutine FAKE
     $(CP,RKI,KK,AK,GK,XK,XJIKB,N,KOLEV,DNRT,NCR,RCHECK,XJIKA,RKA,RKB)
C
C     Rudolf Loeser, 1977 Jan 18
C---- Computes RK(KOLEV) in two ways and selects one, for ROPE.
C     !DASH
      save
C     !DASH
      real*8 AK, CP, DNRT, GK, RCHECK, RKA, RKB, RKI, XJIKA, XJIKB, XK,
     $       dummy
      integer KK, KOLEV, N, NCR
      logical KOOL
C     !DASH
      external BAKE, LAKE, HAISLA, MOVE1, HI, BYE
C
C               CP(NL), RKI(N,NL), RKB(N), GK(KKX), XK(KKX), RCHECK(N),
      dimension CP(*),  RKI(*),    RKB(*), GK(*),   XK(*),   RCHECK(*),
C
C               XJIKB(N,KKX), XJIKA(N,KKX), DNRT(N), RKA(N), AK(KKX)
     $          XJIKB(*),     XJIKA(*)    , DNRT(*), RKA(*), AK(*)
C
      data KOOL /.false./
C
      call HI ('FAKE')
C     !BEG
C---- ( ?  Exhibit the two different JNUs)
      call LAKE   (XJIKA, XJIKB, KK, N, KOLEV)
C---- Method B
      call BAKE   (CP, RKB, KK, AK, GK, XK, XJIKB, N, KOLEV, DNRT,
     $             NCR, KOOL, dummy)
C---- Method A
      call BAKE   (CP, RKA, KK, AK, GK, XK, XJIKA, N, KOLEV, DNRT,
     $             NCR, KOOL, dummy)
C---- Compute RCHECK, and exhibit (?)
      call HAISLA (RKB, RKA, RCHECK, N)
C---- Select final RK
      call MOVE1  (RKB, N, RKI)
C     !END
      call BYE ('FAKE')
C
      return
      end
