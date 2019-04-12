      subroutine ANCHOR
     $(X,IX,W,RKI,IQRK,LU)
C
C     Rudolf Loeser, 1972 Mar 02
C---- Supervises calculation of additional photoionization.
C     !DASH
      save
C     !DASH
      real*8 RKI, W, X
      integer IAKJ, IN, IQPAI, IQRK, IS, ITAUJ, ITAUK, IX, IXCBL, J304I,
     $        JJ304, JJLRJ, JJRKC, JJTKR, LU, M304, MLR, MOX, N, NL
      logical ANY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(28),MLR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(102),JJTKR)
      equivalence (IZOQ( 74),JJRKC)
      equivalence (IZOQ(  5),JJ304)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  3),JJLRJ)
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
      equivalence (KZQ( 42),M304 )
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
      equivalence (LEST(13),J304I)
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
      equivalence (IQQ( 85),IQPAI)
C     !DASH
      external JUSTIN, TIN, ZEPELIN, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               RKI(N,NSL), IQRK(NSL)
      dimension RKI(*),     IQRK(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ITAUJ),(IN( 2),IAKJ ),(IN( 3),ITAUK),(IN( 4),IXCBL )
C     !EJECT
C
      call HI ('ANCHOR')
C     !BEG
      if(MLR.gt.0) then
C       (Get, and allocate, W allotment)
        call JUSTIN    (IN, IS, MOX, 'ANCHOR')
C
C----   Compute for all levels
        ANY = .false.
        call TIN       (N, NL, RKI, IX(JJLRJ), X(JJTKR), X(JJRKC),
     $                  W(ITAUJ), W(IAKJ), IQRK, ANY, W(IXCBL),
     $                  W(ITAUK), X(JJ304), M304, J304I)
        if(ANY.and.(IQPAI.gt.0)) then
C----     Print details
          call ZEPELIN (LU, N, NL, IX(JJLRJ), IQRK, W(ITAUJ), W(IAKJ),
     $                  X(JJTKR), X(JJRKC), RKI)
        end if
C
C       (Give back W allotment)
        call WGIVE     (W, 'ANCHOR')
      end if
C     !END
      call BYE ('ANCHOR')
C
      return
      end
