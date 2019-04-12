      subroutine BREAKER
     $(LU)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Prints a legend, for SHARI.
C     (This is version 2 of BREAKER.)
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('BREAKER')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ','In the tabulation that follows,')
C
        call LINER (1, LU)
        write (LU,101)
  101   format(' ','"IT" tells about the iterative calculation of ',
     $             'the Continuum Source Function, CSF:'/
     $         ' ','=  n (a positive integer) is the number of ',
     $             'iterations required in the normal case;'/
     $         ' ','= 99 if convergence did not occur before the ',
     $             'iteration limit was reached;'/
     $         ' ','= -1 if no iterations were attempted because ',
     $             'values of scattering ratio are out-of-range.')
C
        call LINER (1, LU)
        write (LU,102)
  102   format(' ','"Damping parameter" tells about the CSF ',
     $             'calculation method by giving the two-letter ',
     $             'method code and, in the case of QR,'/
     $         ' ','the value of the damping parameter required for ',
     $             'it.')
C
        call LINER (1,LU)
        write (LU, 103)
  103   format(' ','"Opac. Mult." generally is the value obtained ',
     $             'from the table MLC;'/
     $         ' ','however, a value marked with * is ',
     $             'line-transition-specific.')
      end if
C     !END
      call BYE ('BREAKER')
C
      return
      end
