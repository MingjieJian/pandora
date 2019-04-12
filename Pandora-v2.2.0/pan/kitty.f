      subroutine KITTY
     $(SPHERE,JSAV)
C
C     Rudolf Loeser, 1980 Jul 14
C---- Sets up the switch JSAV, for saving spectral information
C     for later processing.
C     (This is version 4 of KITTY.)
C     !DASH
      save
C     !DASH
      integer IQABS, IQEMS, IQSPC, IQTAS, JSAV
      logical SPEC, SPHERE, SUMM
C     !COM
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
      equivalence (IQQ( 96),IQSPC)
      equivalence (IQQ( 59),IQABS)
      equivalence (IQQ(104),IQEMS)
      equivalence (IQQ(105),IQTAS)
C     !DASH
      external HI, BYE
C
      call HI ('KITTY')
C     !BEG
      JSAV = 0
C
      if(.not.SPHERE) then
        SPEC = IQSPC.gt.0
        SUMM = (IQABS.gt.0).or.(IQEMS.gt.0).or.(IQTAS.gt.0)
        if(SPEC.or.SUMM) then
          JSAV = 1
        end if
      end if
C     !END
      call BYE ('KITTY')
C
      return
      end
