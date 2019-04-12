      subroutine TURIN
     $(MO,LUH,LUD,LUF,LUS,LUQ,LUC)
C
C     Rudolf Loeser, 1996 Apr 18
C---- Sets up logical output units for TULIP, and prints explanation.
C     !DASH
      save
C     !DASH
      integer IQBQD, IQCGP, IQFBO, IQROD, IQROP, IQROS, LUC, LUD, LUF,
     $        LUH, LUQ, LUS, LX, MO, jummy
      character OPT*3
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
      equivalence (IQQ(199),IQROP)
      equivalence (IQQ(120),IQROD)
      equivalence (IQQ( 28),IQFBO)
      equivalence (IQQ(315),IQROS)
      equivalence (IQQ(251),IQCGP)
      equivalence (IQQ(301),IQBQD)
C     !DASH
      external  ZEUS, ONOFF, PRIAM, LINER, HI, BYE
      intrinsic max
C
      call HI ('TURIN')
C     !BEG
      if(MO.gt.0) then
C
        call ZEUS (MO , IQROP, LUH)
        call ZEUS (LUH, IQROD, LUD)
        call ZEUS (LUH, IQFBO, LUF)
        call ZEUS (LUH, IQROS, LUS)
        call ZEUS (LUH, IQBQD, LUQ)
        call ZEUS (MO , IQCGP, LUC)
        LX = max(LUH,LUC)
C     !EJECT
        if(LX.gt.0) then
          call PRIAM (LX, 'RHO AND RBD', 11)
          call LINER (1, LX)
          write (LX,100)
  100     format(' ','Net rate coefficient (RHO) and ratio of ',
     $               'departure coefficients (RBD) for all ',
     $               'radiative transitions.')
          call LINER (2, LX)
C
          call ONOFF (IQROP, jummy, OPT)
          write (LX,101) OPT
  101     format(' ',8X,'Printout option RHBPRNT (',A3,'): print ',
     $               'this note and selected Results (optional)')
          call ONOFF (IQROD, jummy, OPT)
          write (LX,102) OPT
  102     format(' ',8X,'Printout option RHBPRDT (',A3,'): print ',
     $               'explanations and details for radiative ',
     $               'transitions (only if RHBPRNT = on)')
          call ONOFF (IQBQD, jummy, OPT)
          write (LX,103) OPT
  103     format(' ',8X,'Printout option BDQPRDT (',A3,'): print ',
     $               'details of BDQ calculation ',
     $               '(only if RHBPRNT = on)')
          call ONOFF (IQFBO, jummy, OPT)
          write (LX,104) OPT
  104     format(' ',8X,'Printout option BDPRNT  (',A3,'): print ',
     $               'complete BDR, BDJ, BDS ans S* ',
     $               '(only if RHBPRNT = on)')
          call ONOFF (IQROS, jummy, OPT)
          write (LX,105) OPT
  105     format(' ',8X,'Printout option RHBPRSM (',A3,'): print ',
     $               'final sets of Rho and b-ratios ',
     $               '(only if RHBPRNT = on)')
          call ONOFF (IQCGP, jummy, OPT)
          write (LX,106) OPT
  106     format(' ',8X,'Printout option CHKPRNT (',A3,'): print ',
     $               'consistency CHECKs')
          call LINER (1, LX)
        end if
C
      end if
C     !END
      call BYE ('TURIN')
C
      return
      end
