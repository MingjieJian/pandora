      subroutine CONVLVD
     $(DL,CPROF,K,FWHM,BPROF,W1)
C
C     Rudolf Loeser, 2003 Jul 22
C---- Computes a broadened profile by convolving a calculated profile
C     with a Gaussian instrumental profile having a specified FWHM.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, BPROF, CPROF, CRIT, DDL, DL, FWHM, SNORM, SUM,
     $       TRTLN2, W1, ZERO
      integer I, IE, IS, J, K, M
C     !DASH
      external HART
C
C               DL(K), CPROF(K), BPROF(K), W1(K)
      dimension DL(*), CPROF(*), BPROF(*), W1(*)
C
      data ZERO,TRTLN2,CRIT /0.D0, 1.665109222D0, 1.D20/
C
C     !BEG
      A = TRTLN2/FWHM
      do 102 J = 1,K
        IS = 1
        IE = 0
        do 100 I = 1,K
          DDL = DL(I)-DL(J)
          ARG = (A*DDL)**2
          if(ARG.gt.CRIT) then
            if(DDL.le.ZERO) then
              IS = I
            else
              if(IE.eq.0) then
                IE = I
              end if
            end if
            W1(I) = ZERO
          else
            W1(I) = exp(-ARG)
          end if
  100   continue
        if(IE.eq.0) then
          IE = K
        end if
        M = IE-IS+1
        call HART (M, DL(IS), W1(IS), SNORM)
        do 101 I = IS,IE
          W1(I) = CPROF(I)*W1(I)
  101   continue
        call HART (M, DL(IS), W1(IS), SUM)
        BPROF(J) = SUM/SNORM
  102 continue
C     !END
C
      return
      end
