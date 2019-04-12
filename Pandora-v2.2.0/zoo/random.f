      subroutine RANDOM
     $(R)
C     Rudolf Loeser, 1988 Jun 30
C---- Delivers the next member of a sequence of pseudo-random numbers,
C     covering the interval 0 to 1.
C     Computed by 32-bit integer arithmetic. Result is a real*8!
C
C---- Algorithm devised by:  P i e r r e   L ' E c u y e r
C
C     See: L'Ecuyer, P. (1988), "Efficient and Portable Combined
C     Random Number Generators", CACM, Vol. 31, No. 6, p. 742
C
C     See also: Bratley, Fox and Schrage: "A Guide to Simulation",
C     Second Edition, 1987 (Springer, New York).
C     This book contains detailed advice on how to use L'Ecuyer's
C     algorithm to generate disjoint sequences of pseudo-random numbers.
C     !DASH
      save
C     !DASH
      real*8 FAC, R
      integer K, S1, S2
C     !COM
      common /CORAND/ S1,S2,K
C     !DASH
      data FAC /4.656613D-10/
      data S1, S2 /1, 4190521/
C
C     !BEG
C---- Next member of Stream 1
      K  = S1/53668
      S1 = (40014*(S1-(K*53668)))-(K*12211)
      if(S1.lt.0) then
        S1 = S1+2147483563
      end if
C---- Next member of Stream 2
      K  = S2/52774
      S2 = (40692*(S2-(K*52774)))-(K*3791)
      if(S2.lt.0) then
        S2 = S2+2147483399
      end if
C---- Combine the streams
      K = S2-S1
      if(K.le.0) then
        K = K+2147483562
      end if
C
      R = K
      R = R*FAC
C     !END
C
      return
      end
