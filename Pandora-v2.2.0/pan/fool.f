      subroutine FOOL
     $(X,W,IW,ISWA,IMG,LU,RECZ)
C
C     Rudolf Loeser, 1994 May 17
C---- Computes Z-from-TAUKIN, if needed; returns RECZ = .true. if yes.
C     (This is version 5 of FOOL.)
C     !DASH
      save
C     !DASH
      real*8 REFLM, W, X
      integer ICB5, ICBREF, IFINT, IFO, IMG, IN, IP5, IPO, IS, ISWA,
     $        ITAU5, IW, IZO, IZP, JJPRF, JJTKI, JJZ, KTKIN, LU, MOX, N,
     $        NEWZ
      logical RECZ
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ(126),JJPRF)
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
      equivalence (QZQ(  1),QNAME)
      equivalence (RZQ( 91),REFLM)
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
      equivalence (LEST(11),KTKIN)
      equivalence (LEST(34),NEWZ )
C     !DASH
C     !EJECT
      external NIGEL, DEAD, QUARTZ, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               ISWA(Nopac), IMG(N)
      dimension ISWA(*),     IMG(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IPO   ),(IN( 2),ICBREF),(IN( 3),IP5   ),(IN( 4),ICB5  ),
     $(IN( 5),ITAU5 ),(IN( 6),IZP   ),(IN( 7),IZO   ),(IN( 8),IFINT ),
     $(IN( 9),IFO   )
C
      call HI ('FOOL')
C     !BEG
      RECZ = (KTKIN.gt.0).and.(QNAME.eq.'HYDROGEN')
      if(RECZ) then
C       (Get, and allocate, W allotment)
        call NIGEL  (IN, IS, MOX, 'FOOL')
C
C----   Compute and print
        call DEAD   (X, W, IW, N, X(JJTKI), REFLM, X(JJPRF), W(IPO),
     $               W(ICBREF), W(IP5), W(ICB5), W(ITAU5), W(IZP),
     $               X(JJZ), W(IZO), W(IFINT), ISWA, IMG, W(IFO), LU)
        NEWZ = 1
C----   Save for iterative summary
        call QUARTZ (X(JJZ))
C
C       (Give back W allotment)
        call WGIVE  (W, 'FOOL')
      end if
C     !END
      call BYE ('FOOL')
C
      return
      end
