      subroutine CAROL
     $(KHED,LUD,LUA)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Printout control for YARN.
C     (This is version 3 of CAROL.)
C     !DASH
      save
C     !DASH
      integer IQANA, IQDPW, KHED, LU, LUA, LUD, MO
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
      equivalence (IQQ( 25),IQDPW)
      equivalence (IQQ(  1),IQANA)
C     !DASH
      external  ZEUS, CLEF, LINER, HI, BYE
      intrinsic max
C     !EJECT
C
      call HI ('CAROL')
C     !BEG
      call ZEUS    (MO,IQDPW,LUD)
      call ZEUS    (MO,IQANA,LUA)
      LU = max(LUD,LUA)
C
      if((LU.gt.0).and.(KHED.eq.0)) then
        KHED = 1
        call CLEF  (LU)
      end if
C
      if(LUD.gt.0) then
        call LINER (3,LUD)
        write (LUD,100)
  100   format(' ','Final recomputation of Line Broadening for ',
     $             'Profile Calculation.')
      end if
C     !END
      call BYE ('CAROL')
C
      return
      end
