      subroutine TDAWSON
C     Rudolf Loeser, 1990 Dec 05
C---- Part of DRAYSON: Tabulates Dawson's function at mesh points.
C     !DASH
      save
C     !DASH
      real*4 B, C, C0, D0, D1, D2, D3, D4, FAC, FIVE, H, HALF, HN, ONE,
     $       RI, TWO, XI
      integer I, J
      logical KILROY
C     !COM
      common /CDAWSON/ H,HN,RI,D0,D1,D2,D3,D4,KILROY
C     !DASH
      dimension B(22),C(21),HN(25),RI(15),D0(25),D1(25),D2(25),
     $          D3(25),D4(25)
C
      data B(1), B(2) / 0.E0, 7.093602E-8 /
      data C /  7.093602E-8, -2.518434E-7,  8.566874E-7,
     $         -2.787638E-6,  8.660740E-6, -2.565551E-5,
     $          7.228775E-5, -1.933631E-4,  4.899520E-4,
     $         -1.173267E-3,  2.648762E-3, -5.623190E-3,
     $          1.119601E-2, -2.084976E-2,  3.621573E-2,
     $         -5.851412E-2,  8.770816E-2, -1.216640E-1,
     $          1.558400E-1, -1.840000E-1,  2.000000E-1 /
      data FAC / 1.6E-1 /
      data HALF, ONE, TWO, FIVE / 5.E-1, 1.E0, 2.E0, 5.E0 /
C
C     !BEG
      do 101 I=1,25
        XI = I
        HN(I) = H*(XI-HALF)
        C0 = FAC*(HN(I)**2)-TWO
C
        do 100 J = 2,21
          B(J+1) = C0*B(J)-B(J-1)+C(J)
  100   continue
C
        D0(I) =  HN(I)*(B(22)-B(21))/FIVE
        D1(I) =  ONE-TWO*HN(I)*D0(I)
        D2(I) = (HN(I)*D1(I)+D0(I))/RI(2)
        D3(I) = (HN(I)*D2(I)+D1(I))/RI(3)
        D4(I) = (HN(I)*D3(I)+D2(I))/RI(4)
  101 continue
C     !END
C
      return
      end
