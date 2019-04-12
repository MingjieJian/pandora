      subroutine JITNEY
     $(MOVING,SPHERE,INCDNT,JNUMTH)
C
C     Rudolf Loeser, 2003 Jun 25
C---- Sets up Continuum-Jnu-calculation control switches.
C     !DASH
      save
C     !DASH
      integer IQEXA, IQINC, IQNCJ, IQSFS, JNUMTH
      logical INCDNT, MOVING, SPHERE
C     !COM
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
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
      equivalence (IQQ(154),IQNCJ)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 51),IQINC)
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('JITNEY')
C     !BEG
      INIHLL = .true.
C
      MOVING = IQEXA.gt.0
      SPHERE = IQSFS.gt.0
      INCDNT = IQINC.gt.0
C
      JNUMTH = IQNCJ
      if(MOVING) then
        JNUMTH = 1
      end if
C     !END
      call BYE ('JITNEY')
C
      return
      end
