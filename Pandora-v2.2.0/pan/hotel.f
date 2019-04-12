      subroutine HOTEL
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Sets up data for rates integrations wavelengths
C     continuum calculations.
C     (This is version 3 of HOTEL.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IN, IQRK, IQRL, IQUTR, IRCP, IS, ITYPK, ITYPR, IW, IWAV,
     $        IWS, IX, IYR, JJMRJ, JJRRC, JJWRA, JJYRA, JN, JOPAC,
     $        JOPAT, JSTCN, KSHEL, LVL, MOX, MUX, NOION, NSH, NSL, NT
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 77),JJYRA)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  1),JJMRJ)
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
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 35),JSTCN)
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
      equivalence (LEST( 1),KSHEL)
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
      equivalence (IQQ( 53),IQUTR)
C     !DASH
      external JABBER, MORTAIN, SIERRA, GRATE, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWAV ),(IN( 2),IRCP ),(IN( 3),IYR  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC),(JN( 2),JOPAT)
C
      data ITYPR,ITYPK /5, 12/
C     !EJECT
C
      call HI ('HOTEL')
C     !BEG
      DOIT = (IQUTR.le.0).and.(JSTCN.le.0).and.(NOION.le.0)
C
      if(DOIT) then
C       (Get, and allocate, W & IW allotments)
        call JABBER      (IN, IS , MOX, 'HOTEL')
        call MORTAIN     (JN, IWS, MUX, 'HOTEL')
C
C----   Regular levels wavelengths
        do 100 LVL = 1,NSL
          call SIERRA    (X, IX, LVL, IQRK, IQRL)
          DOIT = (IQRK.eq.1).or.(IQRL.eq.1)
C
          if(DOIT) then
            call GRATE   (X, XCBL, LVL,     NSL, ITYPR, W(IWAV),
     $                    W(IRCP), W(IYR), IX(JJMRJ), X(JJWRA),
     $                    X(JJRRC), X(JJYRA), IW(JOPAC), IW(JOPAT),
     $                    SWAVE, SLTIT, NSH)
          end if
  100   continue
C
        if(KSHEL.gt.0) then
C----     K-Shell wavelengths
          call GRATE     (X, XCBL, (NSL+1), NSL, ITYPK, W(IWAV),
     $                    W(IRCP), W(IYR), IX(JJMRJ), X(JJWRA),
     $                    X(JJRRC), X(JJYRA), IW(JOPAC), IW(JOPAT),
     $                    SWAVE, SLTIT, NSH)
        end if
C
C       (Give back W & IW allotments)
        call WGIVE       (W , 'HOTEL')
        call IGIVE       (IW, 'HOTEL')
      end if
C     !END
      call BYE ('HOTEL')
C
      return
      end
