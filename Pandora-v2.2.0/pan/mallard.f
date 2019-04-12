      subroutine MALLARD
     $(CALLER)
C
C     Rudolf Loeser, 1985 Dec 31
C---- Prints contents of LIMBO.
C     !DASH
      save
C     !DASH
      integer IQIXD, LUEO, NOION
      character CALLER*(*)
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
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(174),IQIXD)
C     !DASH
      external MESHED, MASHED, LINER, SOPHIA, HI, BYE
C     !EJECT
C
      call HI ('MALLARD')
C     !BEG
      if((NOION.le.0).and.(IQIXD.gt.0)) then
        call MESHED ('MALLARD', 2)
        call SOPHIA (LUEO)
        call LINER  (1, LUEO)
        write (LUEO,100) CALLER
  100   format(' ','(Option INDXDMP; printed by SOPHIA for ',A,')')
        call MASHED ('MALLARD')
      end if
C     !END
      call BYE ('MALLARD')
C
      return
      end
