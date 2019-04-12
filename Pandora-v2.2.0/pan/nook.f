      subroutine NOOK
     $(KHED,LUD,LUA,LUT,LUP)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Printout control for COOK.
C     !DASH
      save
C     !DASH
      integer IQANA, IQDPW, IQPPR, KHED, LUA, LUD, LUP, LUT, MO
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
      equivalence (IQQ( 42),IQPPR)
      equivalence (IQQ( 25),IQDPW)
      equivalence (IQQ(  1),IQANA)
C     !DASH
      external ZEUS, CLEF, LINER, HI, BYE
C     !EJECT
C
      call HI ('NOOK')
C     !BEG
      LUD = 0
      LUA = 0
      LUT = 0
      LUP = 0
C
      if(IQPPR.gt.0) then
        call ZEUS   (MO,IQDPW,LUD)
        call ZEUS   (MO,IQANA,LUA)
        LUT = 1
        LUP = MO
        if(KHED.eq.0) then
          KHED = 1
          call CLEF (MO)
        end if
      end if
C
      if(LUP.gt.0) then
        call LINER  (3,LUP)
        write (LUP,100)
  100   format(' ','Data for Passive Line Profile Calculation.')
      end if
C     !END
      call BYE ('NOOK')
C
      return
      end
