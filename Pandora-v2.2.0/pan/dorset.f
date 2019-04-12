      subroutine DORSET
     $(LU)
C
C     Rudolf Loeser, 1991 Jun 12
C---- Prints explanation of Lyman EPs massaging procedures.
C     !DASH
      save
C     !DASH
      real*8 WEP, XLMA, XLMB, XLME, XLMF, XLMR, XLMT
      integer IQEN2, IQENL, IQLYA, LU, jummy
      character EN2*3, ENL*3
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 26),XLMT )
      equivalence (RZQ( 33),XLMF )
      equivalence (RZQ( 27),XLME )
      equivalence (RZQ( 35),XLMA )
      equivalence (RZQ( 36),XLMB )
      equivalence (RZQ( 37),XLMR )
      equivalence (RZQ( 40),WEP  )
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
      equivalence (IQQ(255),IQLYA)
      equivalence (IQQ( 71),IQENL)
      equivalence (IQQ( 72),IQEN2)
C     !DASH
C     !EJECT
      external  ABJECT, LINER, ONOFF, HI, BYE
C
      call HI ('DORSET')
C     !BEG
      if((LU.gt.0).and.(IQLYA.le.0)) then
        call ABJECT (LU)
        write (LU,100)
  100   format(' ','About massaging the newly-computed values of ',
     $             'EP1 and EP2:'//
     $         ' ','"Unreasonable" values of EP1 and EP2 can lead to ',
     $             '"unreasonable" (e.g. negative) values of the ',
     $             'Lyman continuum source function,'/
     $         ' ','especially at large TAUK where S is approximately ',
     $             'ERT. We presume that the values of EP1 will be ',
     $             '"reasonable" in a'/
     $         ' ','converged solution. However, erratic, ',
     $             'unreasonable values may arise when a calculation ',
     $             'has not yet converged due to various'/
     $         ' ','numerical difficulties or due to the inherent ',
     $             'lag in achieving complete self-consistency in a ',
     $             'complex PANDORA calculation.'//
     $         ' ','"Unreasonable" interim values of EP1 can ',
     $             'derail an ongoing calculation. Various schemes ',
     $             'have been provided to control'/
     $         ' ','the evolving values of EP1 as the iterations ',
     $             'proceed. The sequence is as follows:')
        call LINER  (1, LU)
        call ONOFF  (IQENL, jummy, ENL)
        write (LU,101) ENL
  101   format(' ','1) EDITH: when option ENL is ON, then negative ',
     $             'values of EP1 are replaced with positive ones '/
     $         ' ','   obtained by interpolation from the nearest ',
     $             'bracketing positive values.'/
     $         ' ','   In this run, ENL is ',A)
        call LINER  (1, LU)
        call ONOFF  (IQEN2, jummy, EN2)
        write (LU,102) EN2
  102   format(' ','2) HEATHER: when option ENL2 is ON, then find EN, ',
     $             'the smallest value of EP1. If EN is negative, then'/
     $         ' ','   (-2 * EN) is added to all EP1 values, and'/
     $         ' ','   (-2 * EN) / BD1  is added to all EP2 values ',
     $             '(using the latest previous BD1 values).'/
     $         ' ','   In this run, ENL2 is ',A)
C     !EJECT
        call LINER  (1, LU)
        write (LU,103) XLMT,XLMF,XLME
  103   format(' ','3) FENN: for TAUK greater than LMT, any EP1 less ',
     $             'than LMF is replaced by LME. This is to replace ',
     $             'negative EP1 values,'/
     $         ' ','   or values too close to zero, with values ',
     $             'similar to neighboring ones, to prevent ',
     $             'unreasonable values of S at large TAUK.'/
     $         ' ','   In this run, LMT =',1PE12.4,', LMF =',E12.4,
     $             ', LME =',E12.4)
        call LINER  (1, LU)
        write (LU,104) XLMA,XLMB,XLMR
  104   format(' ','4) FENN: for TAUK in the range LMA to LMB, any ',
     $             'EP1 value greater than LMR is replaced with LMR. ',
     $             'This is to prevent'/
     $         ' ','   excessively large EP1 values from causing ',
     $             'numerical difficulties.'/
     $         ' ','   In this run, LMA =',1PE12.4,', LMB =',E12.4,
     $             ', LMR =',E12.4)
        call LINER  (1, LU)
        write (LU,105) WEP
  105   format(' ','5) CART: "final" weighted sets of EP1 and EP2 are ',
     $             'computed using the most recent previous sets. ',
     $             'This weighting is linear or'/
     $         ' ','   logarithmic, as appropriate. The weight for ',
     $             'current, just-calculated, as-yet-unweighted, ',
     $             'values is WEP.'/
     $         ' ','   In this run, WEP =',1PE12.4)
        call LINER  (1, LU)
        write (LU,999)
  999   format(' ',94X,'(This text last revised 06/13/91)')
      end if
C     !END
      call BYE ('DORSET')
C
      return
      end
