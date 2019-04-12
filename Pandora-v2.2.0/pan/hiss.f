      subroutine HISS
     $(X,IX)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Prints subiteration heading and input data.
C     (This is version 2 of HISS.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IOMX, IOVER, IQEVR, IQINI, ISUB, ITER, IX, JJAW, JJKIJ,
     $        JJQHI, JJRHO, JJYBR, N, NL, NO, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(242),JJAW )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
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
      equivalence (KZQ(  4),ISUB )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(205),IQINI)
      equivalence (IQQ( 19),IQEVR)
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
C     !DASH
      external SUBHEAD, CRONZI, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('HISS')
C     !BEG
C---- Print heading
      call SUBHEAD  (IOVER, IOMX, ITER, ISUB)
C
      if((IQINI.gt.0).and.(IQEVR.le.0)) then
C----   Print input data
        call CRONZI (NO, N, NL, NT, X(JJYBR), X(JJRHO), X(JJQHI),
     $               X(JJAW), IX(JJKIJ))
      end if
C     !END
      call BYE ('HISS')
C
      return
      end
