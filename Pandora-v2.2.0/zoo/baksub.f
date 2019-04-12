      subroutine BAKSUB
     $(A,L,LDIM,INDX,B)
C
C---- Solves the set of L linear equations A x X = B.
C     Here A is input, not as the matrix A but rather as its
C     L\U decomposition, determined by the routine DECOMP.
C     INDX is input as the permutation vector returned by DECOMP.
C     B is input as the right-hand side vector B, and returns with
C     the solution vector X. A, L and INDX are not modified by
C     this routine and can be left in place for successive calls
C     with different right-hand sides B. This routine takes into
C     account the possibility that B will begin with many zero
C     elements, so it is efficient for use in matrix inversion.
C
C---- BAKSUB is a modified version of LUBKSB, as given in
C     "NUMERICAL RECIPES" BY
C
C     F l a n n e r y ,   H o r o w i t z   and  P r e s s .
C
C---- Modified by R. Loeser, 1983 Jun.
C
C     The input parameter LDIM, LDIM .ge. L, specifies the column
C     dimension of A.
C
C               A(LDIM,LDIM),INDX(LDIM),B(LDIM)     Nominally, but
C               A(LDIM,L),   INDX(L),   B(L)        is sufficient.
C
C     !DASH
      save
C     !DASH
      real*8 A, B, SUM, ZERO
      integer I, II, INDX, IP, J, L, LDIM, N, NDIM
C     !DASH
      dimension A(LDIM,*), INDX(*), B(*)
C
      data ZERO /0.D0/
C     !EJECT
C
C     !BEG
C---- Initialization
      N    = L
      NDIM = LDIM
C---- When II is set to a positive value, it will become the index
C     of the first nonvanishing element of B.
      II = 0
C---- We now do the forward substitution, equation (2.3.6).
C     The only new wrinkle is to unscramble the permutation as we go.
      do 101 I = 1,N
        IP    = INDX(I)
        SUM   = B(IP)
        B(IP) = B(I)
        if(II.ne.0) then
          do 100 J = II,(I-1)
            SUM = SUM-A(I,J)*B(J)
  100     continue
        else if(SUM.ne.ZERO) then
C----     A nonzero element was encountered, so from now on we will
C         have to do the sums in the loop above.
          II = I
        end if
        B(I) = SUM
  101 continue
C---- Now we do the back substitution, equation (2.3.7).
      do 103 I = N,1,-1
        SUM = B(I)
        if(I.lt.N) then
          do 102 J = (I+1),N
            SUM = SUM-A(I,J)*B(J)
  102     continue
        end if
C----   Store a component of the solution vector.
        B(I) = SUM/A(I,I)
  103 continue
C---- All done!
C     !END
C
      return
      end
