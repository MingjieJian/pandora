      subroutine COBRA
     $(KODE,T,XC)
C
C     Rudolf Loeser, 1981 Jan 30
C---- Computes Coulomb cross-sections,
C     for the following interactions, as selected by KODE:
C     KODE = 1  -  H  <-> H;
C     KODE = 2  -  H  <-> proton;
C     KODE = 3  -  H  <-> electron;
C     KODE = 4  -  He <-> He;
C     KODE = 5  -  He <-> He+;
C     KODE = 6  -  He <-> electron;
C     KODE = 7  -  He <-> H;
C     KODE = 8  -  He <-> proton.
C---- Calculated from
C     Nowak and Ulmschneider, Astron.Astrophys., 60, 413 (1977).
C
C---- T is electron temperature (Kelvins).
C     !DASH
      save
C     !DASH
      real*8 B0, B1, B2, T, XC, XL
      integer KODE
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      dimension B0(8), B1(8), B2(8)
C
      data B0 /+6.4539D-15, +2.5585D-14, +1.4204D-14, +4.0748D-15,
     $         +1.2985D-14, -3.9942D-15, +5.2065D-15, +2.0006D-14/
      data B1 /-8.2913D-16, -2.0315D-15, -1.7941D-15, -5.7824D-16,
     $         -1.0798D-15, +1.0713D-15, -7.8033D-16, -3.7342D-15/
      data B2 /+2.3651D-17, +4.0371D-17, +4.8634D-17, +2.0593D-17,
     $         +2.2483D-17, -6.1205D-17, +2.9357D-17, +1.7502D-16/
C
      call HI ('COBRA')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.8)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is outside the range 1 - 8.')
        call HALT ('COBRA', 1)
      end if
C
      XL = log(T)
      XC = B0(KODE)+XL*(B1(KODE)+XL*B2(KODE))
C     !END
      call BYE ('COBRA')
C
      return
      end
