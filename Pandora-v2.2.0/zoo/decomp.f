      subroutine DECOMP
     $(A,L,LDIM,INDX,IPAR,KODE,VV)
C
C---- Given an L x L matrix A, this routine replaces it by the
C     L\U decomposition of a rowwise permutation of itself.
C     A is output, arranged as in equation (2.3.14);
C     INDX is an output vector which records the row permutation
C     effected by the partial pivoting;
C     IPAR is output +1 or -1 depending on whether the number
C     of row interchanges was even or odd, respectively.
C
C---- DECOMP is a modified version of LUDCMP, as given in
C     "NUMERICAL RECIPES" BY
C
C     F l a n n e r y ,   H o r o w i t z   and  P r e s s .
C
C---- Modified by R. Loeser, 1983 Jun.
C
C     The input parameter LDIM, LDIM .ge. L, specifies the column
C     dimension of A. VV is a vector of scratch storage which,
C     like INDX, must be of length at least L.
C
C               A(LDIM,LDIM),INDX(LDIM),VV(LDIM)    Nominally, but
C               A(LDIM,L),   INDX(L),   VV(L)       is sufficient.
C
C---- Upon return, KODE.eq.1 if processing seemed to go OK,
C     KODE.eq.0 if division by 0 was called for during the processing.
C     When KODE.ne.1, A and INDX contain partial results which are
C     unusable.
C     !DASH
      save
C     !DASH
      real*8 A, BIG, DUM, ONE, SUM, VV, ZERO
      integer I, IMAX, INDX, IPAR, J, K, KODE, L, LDIM, N, NDIM
      logical YES
C     !DASH
      external  IS_BAD
      intrinsic abs
C
      dimension A(LDIM,*), INDX(*), VV(*)
C
      data ZERO,ONE /0.D0, 1.D0/
C     !EJECT
C
C     !BEG
C---- Initialization
      N    = L
      NDIM = LDIM
      KODE = 0
C---- Initialize IPAR.
      IPAR = 1
C---- Loop over rows to get the implicit scaling information.
      do 101 I = 1,N
        BIG = ZERO
        do 100 J = 1,N
          if(abs(A(I,J)).gt.BIG) then
            BIG = abs(A(I,J))
          end if
  100   continue
        call IS_BAD (BIG,YES)
        if(YES.or.(BIG.eq.ZERO)) then
C----     If no nonzero largest element, the matrix is singular.
          goto 109
        end if
C----   Save the scaling.
        VV(I) = ONE/BIG
  101 continue
C---- This is the loop over columns of CROUT's method.
      do 108 J = 1,N
        if((J-1).gt.0) then
C----     This is equation (2.3.12) except for i=j.
          do 103 I = 1,(J-1)
            if((I-1).gt.0) then
              SUM = A(I,J)
              do 102 K = 1,(I-1)
                SUM = SUM-A(I,K)*A(K,J)
  102         continue
              A(I,J) = SUM
            end if
  103     continue
        end if
C     !EJECT
C----   Initialize for the search for largest pivot element.
        BIG = ZERO
C----   This is i=j of equation (2.3.12) and i=j+1 ... N of
C       equation (2.3.13).
        do 105 I = J,N
          SUM = A(I,J)
          if((J-1).gt.0) then
            do 104 K = 1,(J-1)
              SUM = SUM-A(I,K)*A(K,J)
  104       continue
            A(I,J) = SUM
          end if
C----     Figure of merit for the pivot.
          DUM = abs(SUM)*VV(I)
C----     Is it better than the best so far?
          if(DUM.gt.BIG) then
            BIG  = DUM
            IMAX = I
          end if
  105   continue
C----   Do we need to interchange rows?
        if(J.ne.IMAX) then
C----     Yes, do so ...
          do 106 K = 1,N
            DUM       = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K)    = DUM
  106     continue
C----     ... and change the sign of IPAR
          IPAR = -IPAR
C----     Also interchange the scale factor.
          VV(IMAX) = VV(J)
        end if
        INDX(J) = IMAX
C----   Now, finally, divide by the pivot element.
        if(J.ne.N) then
          call IS_BAD (A(J,J),YES)
          if(YES.or.(A(J,J).eq.ZERO)) then
C----       If the pivot element is zero the matrix is singular
C           (at least to the precision of the algorithm).
            goto 109
          end if
          DUM = ONE/A(J,J)
          do 107 I = (J+1),N
            A(I,J)=A(I,J)*DUM
  107     continue
        end if
C----   Go back for the next column in the reduction.
  108 continue
C---- Getting here means all seemed OK.
      KODE = 1
  109 continue
C     !END
C
      return
      end
