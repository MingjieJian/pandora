      subroutine WHITE
     $(MVEL,VEL,NVX,WTP)
C
C     Rudolf Loeser, 1990 Dec 18
C---- Writes velocity data to Special Spectrum Save file.
C     (This is version 3 of WHITE.)
C     !DASH
      save
C     !DASH
      real*8 VEL, WTP
      integer IQPPU, LUSO, MODE, MVEL, N, NVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
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
      equivalence (IQQ(151),IQPPU)
C     !DASH
C     !EJECT
      external PANT, PUNT, HI, BYE
C
C               VEL(N,MVEL), WTP(NVX)
      dimension VEL(*),      WTP(*)
C
      data MODE /1/
C
      call HI ('WHITE')
C     !BEG
      if(IQPPU.gt.0) then
        write (LUSO,100)
  100   format('----6  VELOCITIES'/
     $         7X,'Velocity sets as in Printout (i.e. before the ',
     $            'first printed line profile)'/
     $         7X,'WTP are the profile weights for flow broadening')
C
        call PANT (LUSO, VEL, N, MVEL, MODE, 'VEL')
        call PUNT (LUSO, WTP, NVX,     MODE, 'WTP')
      end if
C     !END
      call BYE ('WHITE')
C
      return
      end
