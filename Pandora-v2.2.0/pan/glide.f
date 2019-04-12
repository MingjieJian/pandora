      subroutine GLIDE
     $(X,W,IW,XK,GK,XKX,GKX,IADRS,KKU)
C
C     Rudolf Loeser, 2005 Jul 25
C---- Drives the calculation of augmented Lyman XK and GK tables.
C     !DASH
      save
C     !DASH
      real*8 GK, GKX, W, X, XK, XKX
      integer IADRS, IGANT, IKTYP, IN, IS, IW, IWAVS, IWS, JJXCU, JJXNU,
     $        JN, KKU, MOX, MUX
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(260),JJXCU)
C     !DASH
      external PRIDE, KAISER, KOENIG, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XKX(KKX), GKX(KKX), IADRS(KKX), XK(KK), GK(KK)
      dimension XKX(*),   GKX(*),   IADRS(*),   XK(*),  GK(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IWAVS ),(IN( 2),IGANT )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IKTYP )
C
      call HI ('GLIDE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call KAISER (IN, IS,  MOX, 'GLIDE')
      call KOENIG (JN, IWS, MUX, 'GLIDE')
C
      call PRIDE  (KKU, X(JJXNU), X(JJXCU), XK, GK, XKX, GKX, W(IWAVS),
     $             IADRS, IW(IKTYP), W(IGANT))
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'GLIDE')
      call IGIVE  (IW, 'GLIDE')
C     !END
      call BYE ('GLIDE')
C
      return
      end
