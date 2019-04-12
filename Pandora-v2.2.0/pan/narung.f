      subroutine NARUNG
     $(N,XLM,GNL)
C
C     Rudolf Loeser, 1990 Oct 11
C---- Computes the Hydrogen bound-free Gaunt factor
C     for level N at wavelength XLAM (Angstroms).
C
C     Mathisen, R.  1984, Photo cross-sections for stellar stmosphere
C     calculations - Compilation of references and data,
C     Inst. Theoret. Astrophys., University of Oslo, Pub. series No. 1
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, E, GNL, ONE, XH, XL, XLAM, XLM, XLM2
      integer N
C     !DASH
      external  HI, BYE
      intrinsic max
C
      dimension A(15), B(15), C(15), D(15), E(15), XL(15), XH(15)
C
      data A /
     $ -2.02848D+05,  1.10500D+00,  1.10100D+00,  1.10100D+00,
     $  1.10200D+00,  1.09860D+00, -2.06411D+07, -2.91537D+07,
     $ -5.25160D+07, -4.82529D+07, -5.74871D+07, -8.81478D+07,
     $ -1.30735D+08, -1.51854D+08, -1.90977D+08/
      data B /
     $ -1.31854D+04, -7.92200D-05, -3.29000E-05, -1.92300D-05,
     $ -1.30400D-05, -9.02000D-06,  8.32740D+04,  1.02091D+05,
     $  1.92771D+05,  1.39700D+05,  1.61021D+05,  1.95139D+05,
     $  2.47876D+05,  2.74494D+05,  3.36368D+05/
      data C /
     $ -2.06536D-01,  4.53600D-09,  1.15200D-09,  5.11000D-10,
     $  2.63800D-10,  1.36700D-10,  7.79095D-01,  8.01148D-01,
     $  7.46637D-01,  8.35914D-01,  8.47916D-01,  8.55662D-01,
     $  8.56825D-01,  8.66258D-01,  8.65170D-01/
      data D /
     $  9.36244D+04,  0.00000D+00,  0.00000D+00,  0.00000D+00,
     $  0.00000D+00,  0.00000D+00, -1.57600D+07, -2.30675D+07,
     $ -3.97851D+07, -4.02115D+07, -5.10197D+07, -7.94736D+07,
     $ -1.18616D+08, -1.39892D+08, -1.76141D+08/
      data E /
     $  2.21980D+03,  0.00000D+00,  0.00000D+00,  0.00000D+00,
     $  0.00000D+00,  0.00000D+00,  6.02788D+04,  7.63484D+04,
     $  1.33609D+05,  1.09690D+05,  1.28543D+05,  1.58058D+05,
     $  2.02013D+05,  2.26509D+05,  2.77724D+05/
C
      data XH /
     $  9.12D2, 3.65D3, 8.21D3, 1.46D4, 2.28D4, 3.29D4, 4.47D4, 5.84D4,
     $  7.39D4, 9.12D4, 1.11D5, 1.32D5, 1.55D5, 1.79D5, 2.06D5/
      data XL /
     $  6*1.0D1, 9*3.0D3/
C
      data ONE /1.D0/
C     !EJECT
C
      call HI ('NARUNG')
C     !BEG
      GNL = ONE
C
      if((N.ge.1).and.(N.le.15)) then
        if(XLM.lt.XH(N)) then
          XLAM = max(XLM,XL(N))
          XLM2 = XLAM**2
          if((N.ge.2).and.(N.le.6)) then
            GNL = (A(N)+B(N)*XLAM+C(N)*XLM2)
          else
            GNL = ((A(N)+B(N)*XLAM+XLM2)/(D(N)+E(N)*XLAM+XLM2))*C(N)
          end if
        end if
      end if
C     !END
      call BYE ('NARUNG')
C
      return
      end
