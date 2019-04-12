      subroutine METALIC
     $(N,NMTS,Z,F,ZL,FL,IMAGE,TIT,GRID,NG,NV,NH,KOUNT,IBEG,IEND,XL,XR)
C
C     Rudolf Loeser, 1982 May 14
C---- Plots functions of metals, for OLBIA.
C     (This is version 2 of METALIC.)
C     !DASH
      save
C     !DASH
      real*8 F, FL, GRID, SIG, TEN, XL, XR, Z, ZL, dummy
      integer IBEG, IEND, KOUNT, KOUNTA, KOUNTB, KZOPT, N, NG, NH, NMTS,
     $        NV
      character IMAGE*(*), TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
C     !EJECT
      external LETUP, ZED, TALIMEC, KKOUNT, SHRIMP, HI, BYE
C
C               Z(N), ZL(N), F(N,NMT), FL(N,NMT), GRID(NG)
      dimension Z(*), ZL(*), F(*),     FL(*),     GRID(*)
C
      data KZOPT /0/
C
      call HI ('METALIC')
C     !BEG
      SIG = TEN
C---- Compute and edit logs
      call LETUP   (F, FL, N, NMTS, SIG)
C---- Set up Z points
      IBEG = 0
      IEND = 0
      call ZED     (Z, N, dummy, 0, KZOPT, IBEG, IEND, ZL, TIT,
     $              'METALIC')
      XL = ZL(IBEG)
      XR = ZL(IEND)
C---- Initialize plot image
      call TALIMEC (IMAGE, XL, XR, GRID, NG, NV, NH, TIT)
C---- Enter points
      call KKOUNT  (IMAGE, KOUNTA)
      call SHRIMP  (ZL, N, IBEG, IEND, FL, NMTS, ALPHS, 26, SIG, 2,
     $              IMAGE)
      call KKOUNT  (IMAGE, KOUNTB)
      KOUNT = KOUNTB-KOUNTA
C     !END
      call BYE ('METALIC')
C
      return
      end
