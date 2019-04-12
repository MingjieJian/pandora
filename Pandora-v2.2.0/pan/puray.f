      subroutine PURAY
     $(LU)
C
C     Rudolf Loeser, 2005 Aug 23
C---- Prints a legend for PEACH.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('PURAY')
C     !BEG
      if(LU.gt.0) then
        call LINER  (1, LU)
        call DASHER (LU)
        call LINER  (2, LU)
        write (LU,100)
  100   format(' ','  RRNU: frequency relative to that of continuum ',
     $             'edge.'//
     $         ' ','  RRCP: frequency dependence of photoionization ',
     $             'cross-section.'/
     $         ' ',8X,'RRCP times the threshhold photoionization ',
     $             'cross-section on the ATOM page is the ',
     $             'monochromatic cross-section.')
        call LINER  (1, LU)
        write (LU,101)
  101   format(' ',8X,'Notes: * The photoionization cross-section at ',
     $             'each wavelength in the level L continuum ',
     $             'is RRCP times the'/
     $         ' ',17X,'threshhold value for level L listed on the ',
     $             'ATOM page (above).'/
     $         ' ',15X,'* The photoionization rate RK, which is ',
     $             'calculated as part of the "RATES" ',
     $             'computation (below),'/
     $         ' ',17X,'is the sum of two terms: the integral over ',
     $             'the RRNU table, plus a remainder based on'/
     $         ' ',17X,'a pure nu**-3 variation.'//
     $         ' ','Method: Continuum Source Function calculation ',
     $           'method control parameter.')
        call LINER  (2, LU)
      end if
C     !END
      call BYE ('PURAY')
C
      return
      end
