      subroutine ESNI
     $(NO)
C
C     Rudolf Loeser, 2003 Jun 18
C---- Prints an explanation of POPULATIONS printouts.
C     (This is version 3 of ESNI.)
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external ESIN, LINER, HI, BYE
C
      call HI ('ESNI')
C     !BEG
      if(NO.gt.0) then
        call LINER (3, NO)
        write (NO,100)
  100   format(' ','In each overall iteration there is one primary ',
     $             ' P O P U L A T I O N S  calculation, and also ',
     $             'several secondary ones.'//
     $         ' ','P R I N T O U T   from the primary calculation ',
     $             'depends on option ALL; output'/
     $         ' ','                  from the secondary ones occurs ',
     $             'in the last iteration only.')
C
        call LINER (1, NO)
        write (NO,101)
  101   format(' ',5X,'The basic printout of unedited, unweighted ',
     $             'results, including electron density, is ',
     $             'controlled by POPPRNT.'/
     $         ' ',5X,'A supplemental printout of final edited, ',
     $             'weighted results is controlled by NBPRNT (POPPRNT ',
     $             'must be on).'/
     $         ' ',5X,'Number density plots are controlled by ',
     $             'POPGRAF.')
        call LINER (1, NO)
        write (NO,102)
  102   format(' ',5X,'The detailed trace of BD and ND calculations ',
     $             'at depth # IBNVIEW is controlled by PDETPRNT ',
     $             '(POPPRNT must be on).')
        call LINER (1,NO)
        write (NO,103)
  103   format(' ','A dump of details of the NK and ND calculation ',
     $             'occurs when option LNUMDMP is on (last iteration ',
     $             'only).')
        call LINER (1, NO)
        call ESIN  (NO)
C     !EJECT
        call LINER (2, NO)
        write (NO,104)
  104   format(' ','The following output sections are each also ',
     $             'controlled by option ALL:')
        call LINER (1, NO)
        write (NO,105)
  105   format(' ','  Comparison of B-normal and B-direct will be ',
     $             'shown if either option AMBPRNT or option VELGPRNT ',
     $             'is on.'//
     $         ' ','  A dump of the details of b calculation occurs ',
     $             'if option BDMP is on.'/
     $         ' ','  Dumps from N-editing and from B-editing occur ',
     $             'if JNEDP = 1.'//
     $         ' ','  Standard listings of population-ion Ns and Bs ',
     $             'occur in population-update runs if the '
     $             'corresponding output option is on.'/
     $         ' ','  Upper-level charge exchange results are shown ',
     $             'when option CHXPRNT is on.')
C
        write (NO,106)
  106   format(' ',91X,'(This text last revised 2003 Aug 21)')
      end if
C     !END
      call BYE ('ESNI')
C
      return
      end
