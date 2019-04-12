      subroutine MELBERT
     $(LU)
C
C     Rudolf Loeser, 2004 Apr 09
C---- Tells the LSF story, for TREMBLE.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MELBERT')
C     !BEG
      write (LU,100)
  100 format(' ','S O U R C E   F U N C T I O N   S O L U T I O N'//
     $       ' ','The calculation of S, RHO and JBAR involves ',
     $           'computing integrals over frequencies and ',
     $           'directions. After S has been computed,'/
     $       ' ','RHO and JBAR are obtained using S and integrated ',
     $           'terms. Two methods are available for computing S:')
      call LINER (1,LU)
      write (LU,101)
  101 format(' ','The FULL method for S'//
     $       ' ','    computes S from a matrix equation that ',
     $           'combines integrated terms and statistical ',
     $           'equilibrium terms. The FULL calculation'/
     $       ' ','    is often unsatisfactory when optical depths ',
     $           'are not large; an alternative, DIRECT, has been ',
     $           'provided.')
      call LINER (1,LU)
      write (LU,102)
  102 format(' ','The DIRECT method for S'//
     $       ' ','    computes initial values of JBAR using ',
     $           'integrated terms and S(n), the source function ',
     $           'computed from ratios of number'/
     $       ' ','    densities. Updated, final values of S are then ',
     $           'computed with the equation  ',
     $           'S = (JBAR + EP*BS) / (1 + EP) .')
      call LINER (1,LU)
      write (LU,103)
  103 format(' ','Computed values of S, RHO and JBAR may/will be ',
     $           'edited to amend "bad" (i.e. unphysical) values ',
     $           'that can occur in an'/
     $       ' ','unconverged solution and that may throw the ',
     $           'iterations off track.')
C
      call LINER (1, LU)
      write (LU,200)
  200 format(' ',116X,'2004 May 10')
C     !END
      call BYE ('MELBERT')
C
      return
      end
