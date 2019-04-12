      subroutine PRIDE
     $(KKU,XNU,XNUC,XK,GK,XKX,GKX,WAVES,IADRS,KTYPE,GAUNT)
C
C     Rudolf Loeser, 2005 Jul 25
C---- Sets up augmented tables of XK and GK, for Lyman continuum
C     integration
C     !DASH
      save
C     !DASH
      real*8 GAUNT, GK, GKX, WAVES, XK, XKX, XNU, XNUC
      integer I, IADRS, IGKSW, KK, KKU, KKX, KOLEV, KTYPE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(10),KK )
      equivalence (JZQ(12),KKX)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(78),IGKSW)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external JACOB, CUDDLY, DAMON, REVERSD, HABAKUK, DERE, REVERSI,
     $         HALT, HI, BYE
C
C               GAUNT(Maxdatl), WAVES(KKX), IADRS(KKX), KTYPE(KKX),
      dimension GAUNT(*),       WAVES(*),   IADRS(*),   KTYPE(*),
C
C               XK(KK), GK(KK), XKX(KKX), GKX(KKX), XNU(NSL), XNUC(NSL)
     $          XK(*),  GK(*),  XKX(*),   GKX(*),   XNU(*),   XNUC(*)
C
      call HI ('PRIDE')
C     !BEG
C---- Set up wavelengths table
      call JACOB     (2, 0, KKU, WAVES, IADRS, KTYPE)
      if(KKU.gt.KKX) then
        write (MSSLIN(1),100) KKU,KKX
  100   format('KKU =',I12,', KKX =',I12,': not acceptable.')
        call HALT    ('PRIDE', 1)
      end if
      call REVERSD   (WAVES, 1, KKU)
      call REVERSI   (IADRS, 1, KKU)
      call REVERSI   (KTYPE, 1, KKU)
C
C---- Convert wavelengths to XK (ratios of frequency units)
      do 101 I = 1,KKU
        call DAMON   (WAVES(I), XNU, XNUC, KOLEV, XKX(I))
  101 continue
C
      if(IGKSW.le.0) then
C----   Compute values of extended GK table
        call HABAKUK (KKU, XKX, GKX, XNU, XNUC, KOLEV, GAUNT)
      else
C----   Get extended GK values by parabolic interpolation
        call DERE    (XK, 1, GK, 1, KK, XKX, 1, GKX, 1, KKU, 1)
      end if
C
C---- (?  dump)
      call CUDDLY    (KKU, XKX, GKX, WAVES, IADRS, KTYPE)
C     !END
      call BYE ('PRIDE')
C
      return
      end
