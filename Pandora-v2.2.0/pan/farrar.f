      subroutine FARRAR
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 31
C---- Controls "POST": PANDORA's post-iterations processing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQCCR, IQCHR, IW, IX, KODE, MO, NO, NOION
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(231),IQCHR)
C     !DASH
C     !EJECT
      external AMASIS, HULL, THURSO, BLYTH, CARRON, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data KODE /3/
C
      call HI ('FARRAR')
C     !BEG
      MO = NO
C
      if(NOION.le.0) then
C----   Line post-processing
        call HULL   (X, IX, W, IW)
C
C----   Final background (continuum) calculations for
C       computed profiles
        call AMASIS (X, IX, W, IW)
      end if
C
C---- Continuum Summary
      call THURSO   (X, IX, W, IW)
C
      if((IQCCR.gt.0).or.(IQCHR.gt.0)) then
C----   Cooling and Heating Rates
        call BLYTH  (X, IX, W, IW)
      end if
C
C---- Iterative Summaries
      call CARRON   (X, IX, W, IW)
C     !END
      call BYE ('FARRAR')
C
      return
      end
