      subroutine ASTER
     $(LU,FUDGE,KTRAS,CRIT,ITMX,ITUS,CWU,KNW)
C
C     Rudolf Loeser, 1990 Aug 01
C---- Prints the general heading and explanation, for TULIP.
C     (This is version 2 of ASTER.)
C     !DASH
      save
C     !DASH
      real*8 CHLIM, CHOP, CRIT, CWJ, CWR, CWU, FUDGE, SMP, WMN, WMX,
     $       XINCH, ZERO
      integer ILI, IQWSR, ITMX, ITUS, KNW, KTRAS, LU, N, NIL,
     $        NT
      character BDT*1, RHOT*1
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 10),CWR  )
      equivalence (RZQ( 11),CHOP )
      equivalence (RZQ( 96),CHLIM)
      equivalence (KZQ( 13),ILI  )
      equivalence (KZQ( 14),NIL  )
      equivalence (RZQ( 54),XINCH)
      equivalence (RZQ( 43),WMN  )
      equivalence (RZQ( 55),WMX  )
      equivalence (RZQ( 56),SMP  )
      equivalence (RZQ(149),CWJ  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
C---- TULLY       as of 2004 Mar 17
      integer     MBD,MRHO
      logical     KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C
      common      /TULLY1/ MBD,MRHO
      common      /TULLY2/ KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C     Intermediates for TULIP: RHO & RBD calculation.
C     .
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
      equivalence (IQQ(123),IQWSR)
C     !DASH
      external  JOSEPH, LINER, ABJECT, HI, BYE
      intrinsic abs
C
      dimension BDT(3), RHOT(3)
C
      data BDT,RHOT /'J', 'R', 'Q', 'S', 'J', 'W'/
C
      call HI ('ASTER')
C     !BEG
      if(LU.le.0) goto 199
C
      call JOSEPH (LU)
      write (LU,100)
  100 format(' ',8X,'E x p l a n a t i o n :')
      call LINER  (2, LU)
      write (LU,101)
  101 format(' ','S       is the line source function'/
     $       ' ','JBAR    is the integrated mean intensity'/
     $       ' ','CHI     = [ JBAR * (EP - AW) - (1 + AW) * EP * BS ]',
     $           ' / (1 + EP), (where AW is the diagonal of the ',
     $           'Lambda-1 weight matrix)')
C     !EJECT
      call LINER (1, LU)
      write (LU,102) BDT(MBD+1)
  102 format(' ','RBD     is the ratio of departure coefficients ',
     $           '(upper / lower), computed by several methods --'/
     $       ' ','  RBD*  is RBD',A1,', the final values for this ',
     $           'run.  Set input parameter BDOPT = BDJ, BDR, or BDQ ',
     $           'to select one of these.'//
     $       ' ','  RBDS  : obtained directly from the ',
     $           'corresponding S (printed for reference only)'/
     $       ' ','  RBDJ  : obtained from JBAR only: solving the ',
     $           'statistical equilibrium equations using only ',
     $           '"single" rates'/
     $       ' ','  RBDR  : obtained from RHO and JBAR: solving ',
     $           'statistical equilibrium equations using both ',
     $           '"net" rates (RHO) and'/
     $       ' ','          "single" rates (JBAR), (as selected ',
     $           'by input parameter KRATE)'/
     $       ' ','  RBDQ  : obtained from CHI. ********** Note: ',
     $           'RBDQ should not be used if some computed values ',
     $           'of S are negative.')
      if(FUDGE.gt.ZERO) then
        write (LU,103) FUDGE
  103   format(' ',10X,'(RHO-fudging occurred in this iteration; ',
     $         ' ','the final weight was',1PE11.4,'). **********')
      end if
      call LINER (1, LU)
      write (LU,104) RHOT(MRHO+1),ITUS,ITMX,CRIT,KNW,CWU,CWJ
  104 format(' ','RHO     is the net rate coefficient (net radiative ',
     $           'bracket) defined by  1 - (JBAR / S), ',
     $           'computed by several methods --'/
     $       ' ','  RHO*  is RHO',A1,', the final values for this ',
     $           'run.  Set input parameter RHOPT = RHOS, RHOJ, or ',
     $           'RHOW to select one of these.'//
     $       ' ','  RHOS  : obtained from the Line Source Function ',
     $           'calculation'/
     $       ' ','  RHOJ  = [ CWJ(S/S*) + (1-CWJ) ] RHOS, where S* ',
     $           'is obtained from RBDR [iterations used = ',I2,
     $           ' (limit ',I2,'), CRIT =',1PE8.1,'],'/
     $       ' ','          and, after',I6,'     adjustments, final ',
     $           'CWJ =',E11.4,' (the input, starting value is',
     $           E11.4,').'/
     $       ' ','          The input value of CWJ should be reduced ',
     $           'if the number of iterations reaches the limit')
C     !EJECT
      write (LU,105) CWR,NIL
  105 format(' ','  RHOW    is a combination of RHOJ and RHOS, using ',
     $           'RHOJ in regions that are optically thin. ',
     $           '(For detailed explanations, see '/
     $       ' ',10X,'Chapter 6 by Avrett and Loeser in Kalkofen ',
     $           '(1987) "Numerical Radiative Transfer", and ',
     $           'Program Note "91 Sep 24".)'/
     $       ' ','        = RW * RHOJ + (1 - RW) * RHOS, where ',
     $           'RW = CWR * WT; in this run, CWR =',1PE11.4/
     $       ' ',13X,'WT is obtained from WW as the centered ',
     $           'M-point running average using equal weights'/
     $       ' ',16X,'where M = 2 * NIL + 1; in this run, NIL =',I4/
     $       ' ',13X,'WW = 0 at all depths except those where RHOJ ',
     $           'is used. For those depths WW is set = 1 by ',
     $           'one or more of several'/
     $       ' ',16X,'procedures controlled by ILI, CHOP, CHLIM ',
     $           'and option RHOWOPT.')
      if((ILI.ge.1).and.(ILI.le.N)) then
        write (LU,106) ILI
  106   format(' ',16X,'Since ILI =',I4,' is a valid depth index, ',
     $            'WW is set = 1 at depths # 1 through ILI')
      else
        write (LU,107) ILI,CHOP
  107   format(' ',16X,'Since ILI =',I4,' is not a valid depth index, ',
     $             'WW is set = 1'/
     $         ' ',18X,'at all depths where TAUW is less than CHOP; ',
     $             'in this run, CHOP =',1PE11.4)
        if(abs(CHLIM).ne.ZERO) then
          write (LU,108) CHLIM
  108     format(' ',18X,'and at all depths where [TAUW(i) - ',
     $               'TAUW(i-1)] is less than abs(CHLIM); ',
     $               'in this run, CHLIM =',1PE11.3/
     $           ' ',20X,'(Note: when CHLIM < 0, then the details of ',
     $               'all WT calculations will be printed.)')
        end if
        write (LU,109)
  109   format(' ',18X,'The meaning of TAUW depends on option RHOWOPT:')
        if((KTRAS.gt.0).and.(NT.gt.1)) then
          write (LU,110)
  110     format(' ',18X,'in this run, TAUW at each depth is the ',
     $               'largest of all line-center TAUs at that depth ',
     $               '(printed below).')
        else
          write (LU,111)
  111     format(' ',18X,'in this run, TAUW is the line-center TAU ',
     $               'of each transition.')
        end if
      end if
C     !EJECT
      call LINER (3, LU)
      write (LU,112)
  112 format(' ','The ACTUAL values of RHO that are used are a ',
     $           'WEIGHTED combination of the CURRENT RHO''s and ',
     $           'those from the PREVIOUS iteration.'/
     $       ' ','The values of RHO* printed here are such weighted ',
     $           'values; the weights that were used are printed ',
     $           'as well.')
      call LINER (1, LU)
      write (LU,113)
  113 format(' ','The RHO-weights may be adjusted from iteration to ',
     $           'iteration, as follows:')
      write (LU,114) WMN,XINCH,WMX,SMP
  114 format(' ','  RHO-weights are increased automatically ',
     $           'in steps of INCH, to the maximum value WRMX, ',
     $           'when successive iterative values'/
     $       ' ','  are monotonic; they are decreased in steps of ',
     $           '2*INCH, to WRMN, when oscillatory (zero weight ',
     $           'means no weighting of current).'/
     $       ' ',10X,'In this run, WRMN =',1PE11.4,', INCH =',E11.4,
     $           ', and WRMX =',E11.4//
     $       ' ','  C A U T I O N: when WRMX is large (more than 0.7),',
     $           ' overcorrections sometimes produce oscillations ',
     $           'with growing amplitude.'//
     $       ' ','  SMP smoothes the run of RHO-weights vs. depth ',
     $           '(for no smoothing set SMP =0). In this run, SMP =',
     $           E11.4)
      call LINER (1, LU)
      if(IQWSR.le.0) then
        write (LU,115) 'linearly'
  115   format(' ','  In this run RHO-weighting is done ',A,
     $             '; this is controlled by option WATESTR.')
      else
        write (LU,115) 'logarithmically'
      end if
      write (LU,116)
  116 format(' ',90X,'(This text last revised 2004 Mar 31.)')
      call ABJECT (LU)
C
  199 continue
C     !END
      call BYE ('ASTER')
C
      return
      end
