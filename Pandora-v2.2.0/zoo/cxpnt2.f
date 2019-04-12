      subroutine CXPNT2
     $(X,EI)
C     Rudolf Loeser, 1979 Jan 09
C---- Subroutine for CEXPINT. - Computes
C     EI(XM) for -1. .le. XM .lt. 5.0
C     Power series expansion about zero.
C     !DASH
      save
C     !DASH
      real*8 A1, A10, A11, A12, A13, A14, A15, A16, A17, A2, A3, A4, A5,
     $       A6, A7, A8, A9, ARG, EI, X
C     !DASH
      intrinsic abs
C
      data A1,  A2  /4.1159050D-15, 7.1745406D-14/
      data A3,  A4  /7.6404637D-13, 1.1395905D-11/
      data A5,  A6  /1.7540077D-10, 2.3002666D-09/
      data A7,  A8  /2.7536018D-08, 3.0588626D-07/
      data A9,  A10 /3.1003842D-06, 2.8346991D-05/
      data A11, A12 /2.3148057D-04, 1.6666574D-03/
      data A13, A14 /1.0416668D-02, 5.5555572D-02/
      data A15, A16 /2.5000000D-01, 9.9999999D-01/
      data A17      /5.7721566D-01/
C
C     !BEG
      ARG = log(abs(X))
      EI  = ((((((((((((((((A1*X+A2)*X+A3)*X+A4)*X+A5)*X+A6)*X+A7)*X
     $       +A8)*X+A9)*X+A10)*X+A11)*X+A12)*X+A13)*X+A14)*X+A15)*X
     $       +A16)*X+A17)+ARG
C     !END
C
      return
      end
