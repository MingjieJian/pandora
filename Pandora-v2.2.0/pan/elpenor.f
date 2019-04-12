      subroutine ELPENOR
     $(LU,LUP,LUG,LUS,LUD,LUM,LUX)
C
C     Rudolf Loeser, 1991 May 22
C---- Sets up LUNs & initializes RATES output (if any).
C
C     LUP - regular printout;
C     LUG - graphs;
C     LUS - integration summaries;
C     LUD - ambipolar diffusion;
C     LUM - minimal printout;
C     LUX - header/trailer.
C     !DASH
      save
C     !DASH
      integer IDIF, IQAMB, IQAMD, IQOST, IQRGR, IQRGS, IQUTR, IQVLG,
     $        IQVLP, IRATE, JDIF, LU, LUD, LUG, LUM, LUP, LUS, LUX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ( 24),IQOST)
      equivalence (IQQ(221),IQVLG)
      equivalence (IQQ(239),IQVLP)
      equivalence (IQQ( 95),IQRGR)
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ(309),IQRGS)
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(121),IRATE)
C     !DASH
      external  SUHARD, ZEUS, PRIAM, LINER, DAFANO, HI, BYE
      intrinsic max
C
      call HI ('ELPENOR')
C     !BEG
C---- Basic output LUN (based on NO or MO)
      call SUHARD   (LU)
C
C---- Various specific LUNs
      call ZEUS     (LU, IQOST, LUP)
      if(IQUTR.gt.0) then
        LUG = 0
        LUS = 0
      else
        call ZEUS   (LU, IQRGR, LUG)
        call ZEUS   (LU, IQRGS, LUS)
      end if
      IDIF = max(IQAMD,IQVLG)
      if(IDIF.le.0) then
        JDIF = 0
        LUD  = 0
      else
        JDIF = max(IQAMB,IQVLP)
        call ZEUS   (LU, JDIF, LUD)
      end if
      if((IRATE.ge.1).and.(IRATE.le.N)) then
        LUM = LU
      else
        LUM = 0
      end if
C
      LUX = max(LUP,LUG,LUS,LUD,LUM)
C
      if(LU.gt.0) then
C----   Header and explanations
        call PRIAM  (LU, 'RATES', 5)
        call LINER  (3, LU)
        call DAFANO (LU)
      end if
C     !END
      call BYE ('ELPENOR')
C
      return
      end
