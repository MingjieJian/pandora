      subroutine KHABUR
     $(I,IPJ,ITJ,KAPSMP,NP,NT,NV,C01,C02,C03,C04,IV,CAP)
C
C     Rudolf Loeser, 1989 Aug 28
C---- Computes part of a Composite Line Opacity value, at one velocity.
C     (This is version 2 of KHABUR.)
C     !DASH
      save
C     !DASH
      real*8 C01, C02, C03, C04, CAP, XP0T0, XP0TM, XPMT0, XPMTM
      integer I, IP, IPJ, IT, ITJ, IV, KAPSMP, NP, NT, NV
C     !DASH
      external HI, BYE
C
C               KAMSMP(NP,NT,NV), IPJ(N), ITJ(N), C01(N), C02(N),
      dimension KAPSMP(NP,NT,*),  IPJ(*), ITJ(*), C01(*), C02(*),
C
C               C03(N), C04(N)
     $          C03(*), C04(*)
C
      call HI ('KHABUR')
C     !BEG
      IP = IPJ(I)
      IT = ITJ(I)
C
      XPMTM = KAPSMP(IP-1,IT-1,IV)
      XP0TM = KAPSMP(IP  ,IT-1,IV)
      XPMT0 = KAPSMP(IP-1,IT  ,IV)
      XP0T0 = KAPSMP(IP  ,IT  ,IV)
C
      CAP = C01(I)*XPMTM + C02(I)*XP0TM + C03(I)*XPMT0 + C04(I)*XP0T0
C     !END
      call BYE ('KHABUR')
C
      return
      end
