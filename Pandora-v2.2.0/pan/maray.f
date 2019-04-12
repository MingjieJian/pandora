      subroutine MARAY
     $(NO,IZERO,ZH)
C
C     Rudolf Loeser, 1990 Oct 19.
C---- Prints a transitions data legend, for ATOM.
C     !DASH
      save
C     !DASH
      real*8 CEDMN, CEDMX, CEFEQ, FROSC
      integer ICHSW, IHSSW, IZERO, JDCEA, JDCEG, JDCEI, JDCEJ, JDCER,
     $        JDCES, JDCEV, JDCEW, MCEOF, NO
      logical DEFCE, HYDR, ZH
      character BLANK*1, QELSM*8
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
      equivalence (KZQ(223),MCEOF)
      equivalence (KZQ(123),IHSSW)
      equivalence (RZQ(127),CEFEQ)
      equivalence (RZQ(125),CEDMN)
      equivalence (RZQ(126),CEDMX)
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(122),ICHSW)
      equivalence (RZQ(129),FROSC)
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST( 1),JDCES)
      equivalence (MEST( 2),JDCEG)
      equivalence (MEST( 5),JDCEJ)
      equivalence (MEST( 4),JDCEV)
      equivalence (MEST( 3),JDCEI)
      equivalence (MEST(15),JDCER)
      equivalence (MEST(19),JDCEA)
      equivalence (MEST(28),JDCEW)
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- SUBLET      as of 2006 Dec 04
      character   CITES*64, CQ*20
      dimension   CITES(11), CQ(11)
      common      /SUBLET/ CITES, CQ
C     CI/CE methods source citations for HAMRE et al.
C     .
C     !DASH
      external ALGER, ELGAR, MOROLD, VARAN, KAFUE, LINER, OLDMOR,
     $         HI, BYE
C
      call HI ('MARAY')
C     !BEG
      HYDR  = QELSM.eq.'H  '
C
      call LINER    (1, NO)
      write (NO,100)
  100 format(' ','N O T E S')
      call LINER    (1, NO)
      call VARAN    (NO, IZERO, 'JZATOM')
C     !EJECT
      write (NO,101)
  101 format(' ',8X,'* The collisional de-excitation rate from ',
     $           'upper level U to lower level L is CUL = NE * ',
     $           'CE(T)UL * P(L)/P(U),'/
     $       ' ',10X,'where NE is electron density and P(L), P(U) ',
     $           'are the statistical weights.'/
     $       ' ',10X,'Values of CE(T) pertain to the printed table ',
     $           'of T; CE(T) for other values of T are obtained ',
     $           'by interpolation.'/
     $       ' ',10X,'If only one T value is given, then the one ',
     $           'printed CE(T) value is used for all temperatures.'/
     $       ' ',10X,'(The printed T table is the input table TER, ',
     $           'of length NTE.)')
C
      DEFCE = (JDCEJ+JDCEV+JDCEI+JDCER+JDCES+JDCEG+JDCEW).gt.0
      if(DEFCE) then
        call OLDMOR (NO, 'CE')
        if(JDCES.gt.0) then
          write (NO,102) CITES(7),CQ(7)
  102     format(' ',10X,'- ',A64,5X,A20)
        end if
        if(JDCEG.gt.0) then
          write (NO,102) CITES(8),CQ(8)
        end if
        if(JDCEW.gt.0) then
          write (NO,102) CITES(11),CQ(11)
        end if
        if(JDCEJ.gt.0) then
          write (NO,102) CITES(5),CQ(5)
        end if
        if(JDCEV.gt.0) then
          write (NO,102) CITES(4),CQ(4)
        end if
        if(JDCEI.gt.0) then
          write (NO,102) CITES(9),CQ(9)
          write (NO,103) CEFEQ,CEDMN,CEDMX
  103     format(' ',12X,'(Control parameters for integration: ',
     $               'CEFEQ (accuracy) =',1PE8.1,', CEDMN =',E8.1,
     $               ', CEDMX =',E8.1,'.)')
        end if
        if(JDCER.gt.0) then
          write (NO,102) CITES(10),CQ(10)
          write (NO,104)
  104     format(' ',12X,'In this method CE(T) goes as F(T)/T**(1/2)',
     $              ', where F(T) is a table given by van ',
     $               'Regemorter.')
        end if
      end if
      if(JDCEA.gt.0) then
        call LINER  (1, NO)
        write (NO,105) FROSC
  105   format(' ',8X,'* Default A-values for forbidden transitions ',
     $             'were printed [in brackets].'/
     $         ' ',10X,'They were computed using fraction-of-',
     $             'classical-oscillator-strength FROSC =',1PE12.4)
      end if
C     !EJECT
      call MOROLD  (NO, MCEOF, 'CE')
C
      if(.not.DEFCE) then
        call LINER (1, NO)
        write (NO,106)
  106   format(' ',8X,'* This run does not use any computed default ',
     $             'values of CE.')
      end if
C
      call LINER   (1, NO)
      write (NO,107)
  107 format(' ',8X,'* Rates for transitional collisions with ',
     $           'electrons are computed from ',
     $           'CE(T)*NE*exp(-h*DNU/(k*T)),'/
     $       ' ',10X,'where DNU is the frequency of the transition, ',
     $           'i.e. NU(upper) - NU(lower).'/
     $       ' ',10X,'If any CE(T) is negative, then abs(CE(T)) is ',
     $           'used, and NH is used instead of NE.')
C
      call LINER   (1, NO)
      if(ZH) then
        write (NO,108)
  108   format(' ',8X,'* Collisions with hydrogen atoms are controlled ',
     $             'with input parameters LCH and ICHSW.')
      else
        write (NO,109) ICHSW
  109   format(' ',8X,'* Added to these transitional rates for ',
     $             'collisions with electrons are those for ',
     $             'collisions with hydrogen atoms,'/
     $         ' ',10X,'provided that ICHSW = 1 (here ICHSW =',I2,
     $             '). For transitions for which LCH = 1 for both ',
     $             'the upper and the'/
     $         ' ',10X,'lower level the added rates are initially ',
     $             'Kaulakys'' (see earlier note, above). ',
     $             'A second set of rates is then'/
     $         ' ',10X,'computed from H. W. Drawin, 1969, Z.Physik, ',
     $             '225, 483 for all transitions with lower levels ',
     $             'equal to or smaller'/
     $         ' ',10X,'than the absolute value of LCH(1) (given ',
     $             'earlier, above). (For a transition with A = 0 ',
     $             'the Drawin rate is zero,'/
     $         ' ',10X,'and the Drawin rates involving level 1 are ',
     $             'zero if LCH(1) = 0.) Such Drawin rates replace ',
     $             'the corresponding'/
     $         ' ',10X,'Kaulakys transitional rates.')
      end if
C
      write (NO,110)
  110 format(' ',10X,'See also "About PANDORA," Section 5, Note 136.')
C
      call LINER   (1, NO)
      call ELGAR   (NO, 'CE', DEFCE)
      call LINER   (1, NO)
      call KAFUE   (NO, 'CE')
      call LINER   (1, NO)
C     !EJECT
      if((IHSSW.eq.1).and.HYDR) then
        write (NO,111) BLANK
  111   format(' ',8X,'* The van der Waals',A,'and resonance half ',
     $             'widths listed correspond to a Hydrogen (or atom) ',
     $             'density'/
     $         ' ',10X,'of 10**16, temperature = 5000K, and an ',
     $             'electron density of 10**12.')
        call LINER (1, NO)
        write (NO,112)
  112   format(' ',8X,'* When no value of the Stark halfwidth is ',
     $             'printed and (unused) appears above instead, then'/
     $         ' ',10X,'a Voigt profile without Stark broadening ',
     $             'is convolved with the Stark profile given by'/
     $         ' ',10X,'K. Sutton, 1978, JQSRT, 20, 333 to get the ',
     $             'final profile used in the calculation ',
     $             '(input parameter IHSSW).'/
     $         ' ',10X,'(The parameter "multiplier of default Stark ',
     $             'half width", printed above,'/
     $         ' ',10X,'multiplies Sutton''s parameter "d" ',
     $             '(equation 16).)')
      else
        write (NO,111) ', Stark '
      end if
C
      call LINER   (1, NO)
      write (NO,113)
  113 format(' ',10X,'The damping component selector allows excluding ',
     $           'any of the five damping components for the purpose ',
     $           'of the'/
     $       ' ',10X,'Line Source Function calculation. This is the ',
     $           'input parameter BLCSW whose value is an integer; ',
     $           'it is treated'/
     $       ' ',10X,'and printed as a five-digit binary number. Each ',
     $           'digit controls a component, turning it off ',
     $           'if the digit = 0.'/
     $       ' ',10X,'In order from right to left (i.e. backwards), ',
     $           'the digits control: radiative, v.d. Waals, Stark, ',
     $           'resonance'/
     $       ' ',10X,'and ion broadening respectively (ion broadening ',
     $           'applies only to higher hydrogen lines).')
C
      call LINER   (1, NO)
      write (NO,114)
  114 format(' ',10X,'(Note also the value of "Damping multiplier," ',
     $           'printed below.)')
C
      if(DEFCE) then
        call LINER (1, NO)
        call ALGER (NO)
      end if
C     !END
      call BYE ('MARAY')
C
      return
      end
