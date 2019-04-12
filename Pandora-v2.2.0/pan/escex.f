      subroutine ESCEX
     $(NO,VES,ISB1,ISB2,MSFT,LSFT,EPTAU)
C
C     Rudolf Loeser, 2004 Apr 13
C---- Prints explanation of ESCAPE probability solution.
C     !DASH
      save
C     !DASH
      real*8 EPTAU
      integer IQSOD, ISB1, ISB2, K, LSFT, MSFT, NO
      character LAB*6, VES*3
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
      equivalence (IQQ(203),IQSOD)
C     !DASH
      external  LINER, SLUG, DASHER, HI, BYE
      intrinsic min, max
C
      dimension LAB(2)
C
      data LAB /'Static', 'Moving'/
C     !EJECT
C
      call HI ('ESCEX')
C     !BEG
      call LINER  (1, NO)
C
      K = max(min((ISB1-1),1),0)+1
      write (NO,100) LAB(K),VES
  100 format(' ','The ',A,' Escape Probability approximation was ',
     $           'used, with option VESCAPE = ',A,'.')
C
      if(MSFT.ne.LSFT) then
        write (NO,101) EPTAU
  101   format(' ','(This method was selected automatically because ',
     $             'TAU(2) > ESCTAU =',1PE10.3,'.)')
      end if
C
      if(ISB1.gt.1) then
        call SLUG (NO, ISB1, ISB2)
        if(IQSOD.le.0) then
          write (NO,102)
  102     format(' ','(Option SOBDUMP can be used to control a ',
     $               'detailed dump printout.)')
        end if
      end if
C
      write (NO,103)
  103 format(' ','RHO was computed by one of several approximations, ',
     $           'S from the ratio of number densities, and then ',
     $           'JBAR = S * (1 - RHO).')
C
      call DASHER (NO)
C     !END
      call BYE ('ESCEX')
C
      return
      end
