      subroutine OREGON
     $(SW,KNT,METEP)
C
C     Rudolf Loeser, 1987 Nov 05
C---- Selects method(s) for the "Lyman" epsilons calculation.
C     !DASH
      save
C     !DASH
      integer I, IQEPC, KNT, METEP, MO
      logical SET, SW
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
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
      equivalence (IQQ(  7),IQEPC)
C     !DASH
      external HI, BYE
C
C               SW(KNT)
      dimension SW(*)
C     !EJECT
C
      call HI ('OREGON')
C     !BEG
      SET = .false.
C
      if((IQEPC.gt.0).and.(MO.gt.0)) then
        SET = .true.
      end if
C
      do 100 I = 1,KNT
        SW(I) = SET
  100 continue
C
      if(.not.SET) then
        SW(METEP+1) = .true.
      end if
C     !END
      call BYE ('OREGON')
C
      return
      end
