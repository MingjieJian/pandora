      subroutine PULPIT
     $(KK,XK,AK,GK,TIN)
C
C     Rudolf Loeser, 2005 Jul 11
C---- Prints intermediates for Lyman Source Function calculations.
C     !DASH
      save
C     !DASH
      real*8 AK, GK, TIME, TIN, TOUT, XK, YALYM
      integer I, IQLYA, IQSTA, KK, KKX, KOLEV, LU, MO
      character YMSS*24
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
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
      equivalence (REST( 3),YALYM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !EJECT
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
      equivalence (IQQ( 76),IQSTA)
      equivalence (IQQ(255),IQLYA)
C     !DASH
C     !EJECT
      external ZEUS, LINER, CAMP, SECOND, ABJECT, HI, BYE
C
C               XK(KK), AK(KK), GK(KK)
      dimension XK(*),  AK(*),  GK(*)
C
      call HI ('PULPIT')
C     !BEG
      call ZEUS     (MO, IQSTA, LU)
C
      if((LU.gt.0).and.(IQLYA.le.0)) then
        call CAMP   (YALYM, YMSS)
        call LINER  (3, LU)
        write (LU,100) KOLEV,YMSS,KK,KKX
  100   format(' ','Data for the Lyman continuum integrations',69X,
     $             '(Option STANDARD)'//
     $         ' ','The XK values are frequency relative to the ',
     $             'frequency at the head of the Level',I2,
     $             ' Continuum.'/
     $         ' ','    (Use of an augmented XK table depends on ',
     $             'input parameter KXLYM and option USEWTAB.)'/
     $         ' ','The GK represent the frequency-dependence of the ',
     $             'photoionization cross-section;'/
     $         ' ','    GK times the threshhold ionization cross-',
     $             'section is the monochromatic cross-section.'/
     $         ' ','The AK values are the frequency integration ',
     $             'weights calculated from XK.'//
     $         ' ','For AK:  ',A,5X,'||',33X,I6,10X,'(est. =',I6,')')
  101   format(' ',A6,1X,1P10E12.4/(' ',7X,10E12.4))
C
        call LINER  (1, LU)
        write (LU,101) 'XK    ',(XK(I),I=1,KK)
        call LINER  (1, LU)
        write (LU,101) 'GK    ',(GK(I),I=1,KK)
        call LINER  (1, LU)
        write (LU,101) 'AK    ',(AK(I),I=1,KK)
C
        call SECOND (TOUT)
        TIME = TOUT-TIN
        call LINER  (2, LU)
        write (MO,102) TIME
  102   format(' ','Time =',F13.3,'  sec.')
C
        call ABJECT (LU)
      end if
C     !END
      call BYE ('PULPIT')
C
      return
      end
