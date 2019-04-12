      subroutine GRAHAM
     $(NW,NZE,WAVES,WVNUM,XIS,IND,Z,BMWAC)
C
C     Rudolf Loeser, 1993 Jun 16
C---- Saves intensity/Hz along selected rays
C     !DASH
      save
C     !DASH
      real*8 BMWAC, WAVES, WVNUM, XIS, Z
      integer IND, IQCPU, J, K, LUSO, MODE, NW, NZE
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
      equivalence (IQQ( 97),IQCPU)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C     !DASH
C     !EJECT
      external PUNT, HI, BYE
C
C               WAVES(Nmkuse), WVNUM(Nmkuse), IND(NZE), XIS(Nmkuse,NZE),
      dimension WAVES(*),      WVNUM(*),      IND(*),   XIS(NW,*),
C
C               Z(N)
     $          Z(*)
C
      data MODE /1/
C
      call HI ('GRAHAM')
C     !BEG
      if(IQCPU.gt.0) then
C
        write (LUSO,100) NW,NZE,BMWAC
  100   format('----7  CONTINUUM INTENSITY /HZ FOR SELECTED BEAMS'/
     $         I10,2X,'NW = number of wavelength/wavenumber values'/
     $         I10,2X,'NZE = number of beams',T58,'BMWAC =',1PE15.8)
        call PUNT   (LUSO, WAVES, NW, MODE, 'XLM')
        call PUNT   (LUSO, WVNUM, NW, MODE, 'WVN')
C
        do 102 J = 1,NZE
          K = IND(J)
          write (LUSO,101) J,K,Z(K)
  101     format(2I10,1PE20.12,2X,'ray #, Z-index, Z-value')
          call PUNT (LUSO, XIS(1,J), NW, MODE, 'INT/HZ')
  102   continue
C
      end if
C     !END
      call BYE ('GRAHAM')
C
      return
      end
