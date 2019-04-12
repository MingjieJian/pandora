      subroutine RATTLER
     $(TE,HN1,HNK,HE1N1,HE2N1,HE2NK,XNE,N,NO,XLR,XLE,XLHE,XLH,XLT)
C
C     Rudolf Loeser, 1981 Feb 03
C---- Computes total Thermal Conductivity, XLT, and its
C     components XLR, XLE, XLHE, and XLH.
C     NO is the LUN for optional detailed printout.
C     !DASH
      save
C     !DASH
      real*8 HE1N1, HE2N1, HE2NK, HN1, HNK, TE, XLE, XLH, XLHE, XLR,
     $       XLT, XNE
      integer I, N, NO
C     !DASH
      external GILA, CAIMAN, TUATARA, BOA, VARUNA, HI, BYE
C
C               XLR(N), XLE(N), XLHE(N), XLH(N), XLT(N), TE(N), XNE(N),
      dimension XLR(*), XLE(*), XLHE(*), XLH(*), XLT(*), TE(*), XNE(*),
C
C               HN1(N), HNK(N), HE1N1(N), HE2N1(N), HE2NK(N)
     $          HN1(*), HNK(*), HE1N1(*), HE2N1(*), HE2NK(*)
C
      call HI ('RATTLER')
C     !BEG
C---- Reactive Thermal Conductivity
      call GILA    (TE, HN1, HNK, HE1N1, HE2N1, HE2NK, XNE, XLT, N, XLR)
C---- Translational Thermal Conductivity - Electron
      call CAIMAN  (TE, HN1, HNK, HE1N1, HE2N1, HE2NK, XNE, N,      XLE)
C---- Translational Thermal Conductivity - Helium
      call TUATARA (TE, HN1, HNK, HE1N1, HE2N1, XNE, N,            XLHE)
C---- Translational Thermal Conductivity - Hydrogen
      call BOA     (TE, HN1, HNK, HE1N1, XNE, N,                    XLH)
C
C---- Total Thermal Conductivity
      do 100 I = 1,N
        XLT(I) = XLR(I)+XLE(I)+XLHE(I)+XLH(I)
  100 continue
C
C---- Detailed printout
      call VARUNA  (NO, N, TE, XNE, HN1, HNK, HE1N1, HE2N1, HE2NK,
     $              XLR, XLE, XLHE, XLH, XLT)
C     !END
      call BYE ('RATTLER')
C
      return
      end
