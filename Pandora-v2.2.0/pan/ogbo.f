      subroutine OGBO
     $(X,IX,XNK,XND,N,NL)
C
C     Rudolf Loeser, 1990 Nov 28
C---- Controls calculation of upper level charge exchange data
C     to be used by a Hydrogen run.
C     !DASH
      save
C     !DASH
      real*8 X, XND, XNK
      integer IQCXP, IX, JJCXP, JJCXX, JJLCX, JJNPQ, JJXRK, JJXRL, LU,
     $        MCXK, MO, N, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(214),JJXRK)
      equivalence (IZOQ(215),JJXRL)
      equivalence (IZOQ(216),JJCXP)
      equivalence (IZOQ(217),JJCXX)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 11),JJLCX)
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
      equivalence (IQQ(276),IQCXP)
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
      equivalence (LEST(55),MCXK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ZEUS, OSIN, HI, BYE
C
      dimension X(*), IX(*)
C
C               XNK(N), XND(N,NL)
      dimension XNK(*), XND(*)
C
      call HI ('OGBO')
C     !BEG
      if((MO.gt.0).and.(MCXK.gt.0)) then
        call ZEUS (MO,IQCXP,LU)
        call OSIN (N,NL,XNK,XND,IX(JJNPQ),IX(JJLCX),X(JJCXP),X(JJCXX),
     $             X(JJXRK),X(JJXRL),LU)
      end if
C     !END
      call BYE ('OGBO')
C
      return
      end
