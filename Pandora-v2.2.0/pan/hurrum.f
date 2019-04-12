      subroutine HURRUM
     $(N,DEE,HK,H1,HE1,BETA,HE2K,TE,DRHEAB)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes DRHEAB for THALIA.
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, DRHEAB, H1, HE1, HE2K, HK, ONE, T, TE, TI, X1,
     $       X1L, X2, X2L, X3, X3L, X4, X4L, Y1, Y1I, Y2, Y2I, Y3, Y3I,
     $       Y4, Y4I
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, HI, BYE
C
C               DEE(4,5,N), HK(N), HE1(N), DRHEAB(N), BETA(N), HE2K(N),
      dimension DEE(4,5,*), HK(*), HE1(*), DRHEAB(*), BETA(*), HE2K(*),
C
C               H1(N), TE(N)
     $          H1(*), TE(*)
C
      call HI ('HURRUM')
C     !BEG
      Y1I = HK(1)/H1(1)
      Y2I = BETA(1)/HE1(1)
      Y3I = HE2K(1)/BETA(1)
      Y4I = TE(1)
C
      do 100 I = 2,N
        Y1  = Y1I
        Y2  = Y2I
        Y3  = Y3I
        Y4  = Y4I
        Y1I = HK(I)/H1(I)
        Y2I = BETA(I)/HE1(I)
        Y3I = HE2K(I)/BETA(I)
        Y4I = TE(I)
        X1  = Y1/Y1I
        X1L = log(X1)
        X2  = Y2/Y2I
        X2L = log(X2)
        X3  = Y3/Y3I
        X3L = log(X3)
        X4  = Y4/Y4I
        X4L = log(X1)
 
        T = (DEE(2,1,I)*X1L+DEE(2,3,I)*X2L+DEE(2,4,I)*X3L+
     $       DEE(2,5,I)*X4L)
        call DIVIDE (T,DEE(2,2,I),TI)
C
        DRHEAB(I-1) = exp(-TI)
  100 continue
C
      DRHEAB(N) = ONE
C     !END
      call BYE ('HURRUM')
C
      return
      end
