      subroutine RAVENNA
     $(X,W,Z,GMASIN,GMASS,RECZ)
C
C     Rudolf Loeser, 1978 Sep 07
C---- Supervises new Z calculation.
C     !DASH
      save
C     !DASH
      real*8 DZMSS, FZLIM, GMASIN, GMASS, W, WZM, X, Z, ZQ
      integer IGE, IMIL, IML, IN, IQZCP, IS, IWEIT, IZE, IZN, IZO,
     $        KMASN, MO, MOX, N, NEWZ
      logical RECZ
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ( 59),WZM  )
      equivalence (RZQ( 88),FZLIM)
      equivalence (RZQ( 90),DZMSS)
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
      equivalence (LEST(30),KMASN)
      equivalence (LEST(34),NEWZ )
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
      equivalence (IQQ(326),IQZCP)
C     !DASH
      external JUNGLE, CYRILLO, EULALIA, QUARTZ, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               Z(N), GMASIN(N), GMASS(N)
      dimension Z(*), GMASIN(*), GMASS(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IML   ),(IN( 2),IMIL  ),(IN( 3),IZO   ),(IN( 4),IZN   ),
     $(IN( 5),IGE   ),(IN( 6),IZE   ),(IN( 7),IWEIT )
C     !EJECT
C
      call HI ('RAVENNA')
C     !BEG
      RECZ = (KMASN.gt.0).and.(IQZCP.gt.0)
      if(RECZ) then
C       (Get, and allocate, W allotment)
        call JUNGLE  (IN, IS, MOX, 'RAVENNA')
C
C----   Compute
        call CYRILLO (Z, GMASIN, GMASS, W(IGE), N, WZM, W(IML), W(IMIL),
     $                W(IZO), W(IZN), W(IZE), W(IWEIT), FZLIM, DZMSS,
     $                ZQ)
        NEWZ = 1
C
C----   Print
        call EULALIA (MO, GMASIN, GMASS, W(IGE), W(IZO), W(IZN), Z, WZM,
     $                N, W(IZE), FZLIM, DZMSS, ZQ)
C----   Save for iterative summary
        call QUARTZ  (Z)
C
C       (Give back W allotment)
        call WGIVE   (W, 'RAVENNA')
      end if
C     !END
      call BYE ('RAVENNA')
C
      return
      end
