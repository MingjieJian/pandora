      subroutine ISIS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 17 Jun 71
C---- Drives OSIRIS, to calculate H- departure coefficient.
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer IAHM, IDEL, IEPS, IG, IGP, IHN1, IHN1L, IHNDL, IN, IND,
     $        IOMX, IOVER, IQCCR, IQHMJ, IRD, IRDC, IRM, IRMC, IS, IW,
     $        IWAV, IWS, IX, IXCBL, IXNEL, IXNHML, IXPBL, IYAY, IZL,
     $        JJAHM, JJBHM, JJCRH, JJHMN, JJHND, JJLHM, JJTE, JJXNE,
     $        JJZ, JN, MHL, MJ, MO, MOX, MUX, N, NW, jummy
      logical KOOL
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
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 85),JJAHM)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 63),JJCRH)
      equivalence (IZOQ(267),JJHMN)
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
      equivalence (KZQ(  8),IOMX )
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 5),MHL  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(133),IQCCR)
      equivalence (IQQ(182),IQHMJ)
C     !DASH
C     !EJECT
      external NILE, POPIO, POPUTIL, IGIVE, OSIRIS, WGIVE, PHYLA, ZEUS,
     $         BAVENO, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(19)
      equivalence
     $(IN( 1),IRM   ),(IN( 2),IRD   ),(IN( 3),IGP   ),(IN( 4),IDEL  ),
     $(IN( 5),IEPS  ),(IN( 6),IG    ),(IN( 7),IYAY  ),(IN( 8),IRDC  ),
     $(IN( 9),IHN1L ),(IN(10),IAHM  ),(IN(11),IRMC  ),(IN(12),IWAV  ),
     $(IN(13),IXCBL ),(IN(14),IXPBL ),(IN(15),IHN1  ),(IN(16),IZL   ),
     $(IN(17),IXNEL ),(IN(18),IHNDL ),(IN(19),IXNHML)
C
      dimension JN(1)
      equivalence
     $(JN( 1),IND   )
C
      call HI ('ISIS')
C     !BEG
C     (Get, and allocate, W & IW allotment)
      call NILE    (IN, IS,  MOX, 'ISIS')
      call PHYLA   (JN, IWS, MUX, 'ISIS')
C     (Initialize populations buffer)
      call POPIO   ('INIT', jummy, W(IXPBL))
C
      call ZEUS    (MO, IQHMJ, MJ)
C
      call POPUTIL (W(IXPBL), 1, 1, W(IHN1), 0, dummy, 0,dummy,
     $              0, dummy)
      call BAVENO  (X(JJLHM), X(JJAHM), W(IWAV), W(IAHM), IW(IND), NW)
C
      KOOL = ((IQCCR.gt.0).and.(IOVER.eq.IOMX))
C
      call OSIRIS  (MO, MJ, NW, W(IWAV), W(IAHM), IW(IND), X(JJBHM),
     $              N, X(JJXNE), W(IHN1), X(JJTE), W(IRM), W(IRD),
     $              W(IGP), W(IDEL), W(IEPS), W(IG), W(IYAY),
     $              W(IXCBL), X(JJHMN), X(JJZ), X(JJHND), KOOL,
     $              X(JJCRH), W(IRMC), W(IRDC), W(IZL), W(IXNEL),
     $              W(IHNDL), W(IXNHML), W(IHN1L))
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'ISIS')
      call IGIVE   (IW, 'ISIS')
C     !END
      call BYE ('ISIS')
C
      return
      end
