      subroutine HADRE
     $(IQLYM,KK,XK,GK,XNU,XNUC,KOLEV,GAUNT)
C
C     Rudolf Loeser, 1974 Dec 12
C---- Controls the computation of Lyman GK.
C     !DASH
      save
C     !DASH
      real*8 GAUNT, GK, XK, XNU, XNUC
      integer IGKSW, IQLYM, KK, KOLEV, NOION
      logical IGK
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
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
C     !DASH
      external NAUGHTD, HABAKUK, HI, BYE
C
C               XK(KK), GK(KK), XNU(NSL), XNUC(NSL), GAUNT(MAXDATL)
      dimension XK(*),  GK(*),  XNU(*),   XNUC(*),   GAUNT(*)
C
      call HI ('HADRE')
C     !BEG
      IGKSW = 1
      if((IQLYM.gt.0).and.(NOION.le.0)) then
        call NAUGHTD   (GK, 1, KK, IGK)
C
        if(IGK) then
          IGKSW = 0
          call HABAKUK (KK, XK, GK, XNU, XNUC, KOLEV, GAUNT)
        end if
C
      end if
C     !END
      call BYE ('HADRE')
C
      return
      end
