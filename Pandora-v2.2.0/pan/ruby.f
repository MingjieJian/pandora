      subroutine RUBY
     $(Y,KOUNT,KOUNTS,KOUNTF)
C
C     Rudolf Loeser, 1987 Mar 02
C---- Checks whether the value of Y seems appropriate;
C     if not, sets Y to a better value, and
C     increments appropriate change counters.
C     (This is version 3 of RUBY.)
C     !DASH
      save
C     !DASH
      real*8 ONE, THREE, TWO, Y, ZERO
      integer IQEXA, IQFIN, IQSFS, KOUNT, KOUNTF, KOUNTS
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
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 49),IQFIN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('RUBY')
C     !BEG
      if(IQSFS.gt.0) then
        if(Y.ne.-THREE) then
          Y      = -THREE
          KOUNT  = KOUNT+1
          KOUNTS = KOUNTS+1
        end if
      else
        if(IQFIN.gt.0) then
          if((Y.eq.-TWO).or.((Y.ge.ZERO).and.(Y.le.ONE))) then
            Y      = -ONE
            KOUNT  = KOUNT+1
            KOUNTF = KOUNTF+1
          end if
        end if
      end if
      if(IQEXA.gt.0) then
        if(Y.ne.-THREE) then
          Y      = -THREE
          KOUNT  = KOUNT+1
          KOUNTS = KOUNTS+1
        end if
      end if
C     !END
      call BYE ('RUBY')
C
      return
      end
