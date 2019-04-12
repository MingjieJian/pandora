      subroutine RUIN
     $(X,IX)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Controls calculation of Stimulated Emission factors.
C     !DASH
      save
C     !DASH
      real*8 X, XNUK
      integer IQCSW, IQSTI, IX, JJALF, JJBAT, JJBTL, JJBTR, JJTE, JJTR,
     $        JJXNU, N, NL, NLB, NO, NOUT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 8),NLB)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ( 43),JJBTR)
      equivalence (IZOQ(137),JJBTL)
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
      equivalence (RZQ(  9),XNUK )
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
      equivalence (IQQ( 23),IQSTI)
      equivalence (IQQ( 14),IQCSW)
C     !DASH
      external ZEUS, ROVER, HI, BYE
C
      dimension X(*), IX(*)
C
C
      call HI ('RUIN')
C     !BEG
      call ZEUS  (NO, IQSTI, NOUT)
      call ROVER (N, NL, NLB, NOUT, IQCSW, XNUK, X(JJXNU), X(JJTE),
     $            X(JJTR), X(JJALF), X(JJBAT), X(JJBTR), X(JJBTL))
C     !END
      call BYE ('RUIN')
C
      return
      end
