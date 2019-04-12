      subroutine BLIND
     $(WANT,Y)
C
C     Rudolf Loeser, 1987 Mar 02
C---- Sets up a default value of Y, the L.S.F. method selector.
C     (This is version 3 of BLIND.)
C     !DASH
      save
C     !DASH
      real*8 ONE, THREE, WANT, Y
      integer IQEXA, IQFIN, IQSFS
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ(169),IQEXA)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('BLIND')
C     !BEG
      Y = WANT
C
      if(IQSFS.gt.0) then
        Y = -THREE
      else
        if(IQFIN.gt.0) then
          Y = -ONE
        end if
      end if
C
      if(IQEXA.gt.0) then
        Y = -THREE
      end if
C     !END
      call BYE ('BLIND')
C
      return
      end
