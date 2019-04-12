      subroutine DAFANO
     $(LU)
C
C     Rudolf Loeser, 1991 Jun 03
C---- Prints explanation of RATES printout options.
C     (This is version 2 of DAFANO.)
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external ABJECT, FANADO, LINER, HI, BYE
C
      call HI ('DAFANO')
C     !BEG
      if(LU.gt.0) then
        write (LU,100)
  100   format(' ','The "RATES" calculations include:'//
     $         ' ','CK:  collisional ionization rate, due to ',
     $             'electrons, and Hydrogen atoms (optional),'/
     $         ' ','RK:  photoionization rate,'/
     $         ' ','RL:  corresponding photorecombination rate ',
     $             '(same as RK when J = B),'/
     $         ' ','CIJ: collisional transition rate, due to ',
     $             'electrons, and Hydrogen atoms (optional),'/
     $         ' ','PIJ: bound-free-bound transition rate (rate ',
     $             'from I to J through the continuum).')
        call LINER  (2, LU)
        write (LU,101)
  101   format(' ','Whether and how much  P R I N T O U T  there is ',
     $             'depends on various options and input parameters.'//
     $         ' ','The   B A S I C   controls are:'//
     $         ' ','     LUP (=on when RATEPRNT is on),  LUG (=on ',
     $             'when RATEGRAF is on),  LUD (=on when either ',
     $             'AMBPRNT or VLGPRNT are on), '/
     $         ' ','     LUS (=on when RATESUMM is on),  LUM (=on ',
     $             'when a value of IRATE is specified such that '
     $             '1 .le. IRATE .le. N).'//
     $         ' ','When LUM is on, only the values of CK, RK, RL, ',
     $             'CIJ and PIJ at depth IRATE are printed; this is ',
     $             'intended as a'/
     $         ' ','"minimal" alternative to the voluminous output '
     $             'controlled by LUP.'/
     $         ' ','When LUS is on, a basic summary of the relevant ',
     $             'continuum results is printed; this is intended '
     $             'as an'/
     $         ' ','alternative to potentially much more voluminous ',
     $             'dump printouts.')
        call LINER  (1, LU)
        call FANADO (LU)
        call LINER  (1,LU)
        write (LU,102)
  102   format(' ','(There are also optional dump printouts, too ',
     $             'numerous to summarize here, which depend on ',
     $             'input parameters and options.'/
     $         ' ','These can appear anywhere within the normal ',
     $             'sequence of printouts, described below.)')
C     !EJECT
        call ABJECT (LU)
        write (LU,103)
  103   format(' ','The various component calculations and the ',
     $             'corresponding printouts occur in the following ',
     $             'order:')
        call LINER  (1, LU)
        write (LU,104)
  104   format(' ','1)   Collisions with Hydrogen: printout depends ',
     $             'on LUP and COLHPRNT.')
        call LINER  (1, LU)
        write (LU,105)
  105   format(' ','2)   Rates based on fast electrons: depends on ',
     $             'FELEC; printout depends on LUP, and on PEDDMP ',
     $             'and FELEPRNT.')
        call LINER  (1, LU)
        write (LU,106)
  106   format(' ','3)   GM; printout depends on LUP.')
        call LINER  (1, LU)
        write (LU,107)
  107   format(' ','4)   CK (Note: CK is printed with RK and RL, ',
     $             'below):'/
     $         ' ','     -  compute normal electron collisions'/
     $         ' ','     -  replace with fast electrons results (if ',
     $             'and as needed, if FELE is on); printout depends ',
     $             'on LUP only'/
     $         ' ','     -  add input term CKADD'/
     $         ' ','     -  add term due to collisions with Hydrogen ',
     $             'atoms (if and as needed, if ICHSW = 1).')
        call LINER  (1, LU)
        write (LU,108)
  108   format(' ','5a)  RK and RL if USETRIN is on: calculation ',
     $             'uses radiation temperatures; printout depends ',
     $             'on LUP.'/
     $         ' ','     (There is additional printout if CALCOOL ',
     $             'is on.)')
        call LINER  (1, LU)
        write (LU,109)
  109   format(' ','5b)  RK and RL if USETRIN is off: calculation ',
     $             'integrates the computed background radiation ',
     $             'field; printout depends on LUP.'/
     $         ' ','     (There is additional printout if CALCOOL ',
     $             'is on.)'/
     $         ' ','     *** The amount of printout from the ',
     $             'integration also depends on RATEALL and RATEFULL.')
        call LINER  (1, LU)
        write (LU,110)
  110   format(' ','   ###   Listings of wavelengths used in ',
     $             'continuum integration for each level, depend on ',
     $             'LUS.'/
     $         ' ','   >>>>> Graphs of JNU vs. RNU for each level, ',
     $             'followed by several summary graphs, depend on ',
     $             'LUG.')
C       !EJECT
        call LINER  (1, LU)
        write (LU,111)
  111   format(' ','5c)  Additional effects on RK and/or RL:'/
     $         ' ','     -  additional photoionization (if any LR ',
     $             'is greater than 0); printout depends on LUP and ',
     $             'APHIPRNT'/
     $         ' ','     -  K-shell ionization (if CP(NSL+1) is not ',
     $             'equal 0); printout depends on LUP'/
     $         ' ','     -  additional recombination (if any YK is ',
     $             'not equal to 0); printout depends on LUP'/
     $         ' ','     -  artificial RK(1) enhancement (if RK1INCR ',
     $             'is on); printout depends on LUP'/
     $         ' ','     -  lower-level charge exchange (if CHEXLO ',
     $             'is on); printout depends on LUP'/
     $         ' ','     -  upper-level charge exchange (if CHEXUP ',
     $             'is on and there are eligible levels); printout ',
     $             'depends on CHXPRNT.')
        call LINER  (1, LU)
        write (LU,112)
  112   format(' ','6)   Ionization terms QU and QS: printout ',
     $             'depends on LUP.')
        call LINER  (1, LU)
        write (LU,113)
  113   format(' ','7)   CIJ: printout depends on LUP and CIJPRNT.'/
     $         ' ','     -  compute normal electron collisions'/
     $         ' ','     -  replace with fast electrons results (if ',
     $             'and as needed, if FELE is on); printout depends ',
     $             'on LUP only'/
     $         ' ','     -  add input term CIJADD'/
     $         ' ','     -  add term due to collisions with Hydrogen ',
     $             'atoms (if and as needed, if ICHSW = 1).')
        call LINER  (1, LU)
        write (LU,114)
  114   format(' ','8)   Diffusion calculations: printout depends ',
     $             'on LUD.')
        call LINER  (1, LU)
        write (LU,115)
  115   format(' ','9)   PIJ: printout depends on LUP and PIJPRNT.')
        call LINER  (1, LU)
        write (LU,116)
  116   format(' ','10)  RIJ: printout depends on LUP and RIJPRNT.')
        call LINER  (1, LU)
        write (LU,117)
  117   format(' ','11)  Recombination calculation: printout depends ',
     $             'only on option RCOMPRNT.')
        call LINER  (2, LU)
        write (LU,118)
  118   format(' ',90X,'(This text last revised 2004-Dec-09.)')
      end if
C     !END
      call BYE ('DAFANO')
C
      return
      end
