      subroutine LYNEOP
     $(N,NP,NT,NV,IPJ,ITJ,IVJ,KAPSMP,C01,C02,C03,C04,C05,C06,CLOP)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Computes a raw value of Composite Line Opacity.
C     Based on "LINOP" of R. Kurucz, May 1983.
C     !DASH
      save
C     !DASH
      real*8 C01, C02, C03, C04, C05, C06, CAP, CAP5, CAP6, CLOP, TEN,
     $       ZERO
      integer I, IPJ, ITJ, IV, IVJ, KAPSMP, N, NP, NT, NV
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external KHABUR, HI, BYE
C
C               IPJ(N), ITJ(N), IVJ(N), C01(N), C02(N), C03(N), C04(N),
      dimension IPJ(*), ITJ(*), IVJ(*), C01(*), C02(*), C03(*), C04(*),
C
C               C05(N), C06(N), CLOP(N), KAPSMP(NP,NT,NV)
     $          C05(*), C06(*), CLOP(*), KAPSMP(*)
C
      call HI ('LYNEOP')
C     !BEG
      do 100 I = 1,N
        IV = IVJ(I)
C
        CAP5 = ZERO
        CAP6 = ZERO
C
        if(C05(I).ne.ZERO) then
          call KHABUR (I,IPJ,ITJ,KAPSMP,NP,NT,NV,C01,C02,C03,C04,IV,
     $                 CAP5)
        end if
        if(C06(I).ne.ZERO) then
          call KHABUR (I,IPJ,ITJ,KAPSMP,NP,NT,NV,C01,C02,C03,C04,IV-1,
     $                 CAP6)
        end if
C
        CAP     = C05(I)*CAP5+C06(I)*CAP6
        CLOP(I) = TEN**CAP
  100 continue
C     !END
      call BYE ('LYNEOP')
C
      return
      end
