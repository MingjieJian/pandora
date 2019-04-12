      subroutine FISCAL
     $(X,N,NMAX,BND,SKALEC,SKALER,VECT,ITMX,NO,LABEL)
C     Rudolf Loeser, 1983 Dec 20
C---- Finds column and row scale factors for the matrix X(N,N),
C     subject to the overflow criterion BND, (BND .gt. 0).
C     Will iterate ITMX times.
C     Will print current scale factors for each iteration,
C     to LUN NO, if NO .gt. 0.
C     !DASH
      save
C     !DASH
      real*8 BND, FAC, ONE, SKALEC, SKALER, VECT, X
      integer I, IBIG, ITER, ITMX, J, JBIG, N, NMAX, NO
      character LABEL*50
C     !DASH
      external  FACET, UPSHOT, LINER
C
      dimension X(NMAX,N), SKALEC(N), SKALER(N), VECT(N)
C
      data ONE /1.D0/
C
C     !BEG
      if(N.gt.0) then
C----   Initialize scale factor arrays with first estimates
        do 100 I = 1,N
          SKALEC(I) = ONE
          SKALER(I) = ONE
  100   continue
C
C
C----   Iterative loop for improving scale factors estimates
        do 109 ITER=1,ITMX
C
C----     Loop over columns
          do 102 J=1,N
C----       Pull out scaled j'th column
            do 101 I=1,N
              VECT(I) = X(I,J)*SKALER(I)*SKALEC(J)
  101       continue
C----       Compute scale factor adjustment
            call FACET (VECT,N,FAC,IBIG,BND)
C----       Update scale factor for j'th column
            call UPSHOT (SKALEC(J),FAC,X(IBIG,J),BND)
  102     continue
C
C----     Loop over rows
          do 104 I=1,N
C----       Pull out scaled i'th row
            do 103 J=1,N
              VECT(J) = X(I,J)*SKALEC(J)*SKALER(I)
  103       continue
C----       Compute scale factor adjustment
            call FACET (VECT,N,FAC,JBIG,BND)
C----       Update scale factor for i'th row
            call UPSHOT (SKALER(I),FAC,X(I,JBIG),BND)
  104     continue
C     !EJECT
C----     Print
          if(NO.gt.0) then
            call LINER (3,NO)
            write (NO,105) ITER,ITMX,LABEL
  105       format(' ','Scale Factors computation: Iteration ',I2,
     $             ' of ',I2/' ',A50)
            call LINER (1,NO)
            write (NO,106)
  106       format(' ','Column Scale Factors')
            call LINER (1,NO)
            write (NO,107) (SKALEC(J),J=1,N)
  107       format(' ',7X,1P10E12.4)
            call LINER (1,NO)
            write (NO,108)
  108       format(' ','Row Scale Factors')
            call LINER (1,NO)
            write (NO,107) (SKALER(I),I=1,N)
          end if
C
C----   End of iterative loop
  109   continue
      end if
C     !END
C
      return
      end
