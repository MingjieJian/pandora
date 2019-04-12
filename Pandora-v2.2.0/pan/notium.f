      subroutine NOTIUM
     $(N,LG,CI,XA,WN,XM)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes an XM matrix, for angle-dependent Continuum Calculations.
C     (See also HESTIA.)
C     !DASH
      save
C     !DASH
      real*8 CI, WN, XA, XM
      integer LG, M, N, N2
C     !DASH
      external ZERO1, ARTEMIS, NEGATE, UNITADD, HI, BYE
C
C               CI(N,LG), XA(N,N,LG), WN(N,N,LG), XM(N,N)
      dimension CI(N,*),  XA(N,N,*),  WN(N,N,*),  XM(*)
C
      call HI ('NOTIUM')
C     !BEG
C---- Initialize
      N2 = N**2
      call ZERO1     (XM,N2)
C---- Accumulate angle sums
      do 100 M = 1,LG
        call ARTEMIS (N,N,CI(1,M),XA(1,1,M),WN(1,1,M),XM)
  100 continue
C---- Final subtraction from unit matrix
      call NEGATE    (XM,N2)
      call UNITADD   (1,N,XM,N)
C     !END
      call BYE ('NOTIUM')
C
      return
      end
