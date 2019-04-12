      subroutine LIED
     $(LU,NBAD)
C
C     Rudolf Loeser, 2003 Jun 20
C---- Prints a heading, for EBISSA.
C     !DASH
      save
C     !DASH
      integer LU
      logical NBAD
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('LIED')
C     !BEG
      if(LU.gt.0) then
        call LINER   (2,LU)
        call DASHER  (LU)
        call LINER   (4,LU)
        write (LU,100)
  100   format(' ','Basic listing of Number Densities and ',
     $             'Departure Coefficients for all levels J.'//
     $         ' ','Unmodified computed values of B, N, and NION ',
     $             'are printed (quantities marked with * are L.T.E. ',
     $             'results).')
C
        if(NBAD) then
          call LINER (1,LU)
          write (LU,101)
  101     format(' ',20X,'T h e r e   a r e   B A D   n u m b e r ',
     $               '  d e n s i t y   v a l u e s !')
          call LINER (1,LU)
        end if
      end if
C     !END
      call BYE ('LIED')
C
      return
      end
