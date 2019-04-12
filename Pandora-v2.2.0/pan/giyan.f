      subroutine GIYAN
     $(J,N,NP,NT,NV,NCP,IPJ,ITJ,IVJ,KAPSMP,C01,C02,C03,C04,C05,C06,
     $ ARRCO,DEN,CLOP,FABD)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Computes Composite Line Opacity at a given wavelength.
C     !DASH
      save
C     !DASH
      real*8 ARRCO, C01, C02, C03, C04, C05, C06, CLOP, DEN, FABD
      integer I, IPJ, ITJ, IVJ, J, KAPSMP, N, NCP, NP, NT, NV
C     !DASH
      external LYNEOP, HI, BYE
C
C               ARRCO(NCP,N), KAPSMP(NP,NT,NV), IPJ(N), ITJ(N), IVJ(N),
      dimension ARRCO(NCP,*), KAPSMP(*),        IPJ(*), ITJ(*), IVJ(*),
C
C               C01(N), C02(N), C03(N), C04(N), C05(N), C06(N), DEN(N),
     $          C01(*), C02(*), C03(*), C04(*), C05(*), C06(*), DEN(*),
C
C               CLOP(N)
     $          CLOP(*)
C
      call HI ('GIYAN')
C     !BEG
      call LYNEOP (N,NP,NT,NV,IPJ,ITJ,IVJ,KAPSMP,C01,C02,C03,C04,C05,
     $             C06,CLOP)
C
      do 100 I = 1,N
        ARRCO(J,I) = FABD*DEN(I)*CLOP(I)
  100 continue
C     !END
      call BYE ('GIYAN')
C
      return
      end
