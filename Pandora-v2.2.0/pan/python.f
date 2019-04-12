      subroutine PYTHON
     $(KODE,T,P,CROSS)
C
C     Rudolf Loeser, 1981 Jan 29
C---- Computes Coulomb cross-sections,
C     for the following interactions, as selected by KODE:
C     KODE = 1  -  electron <-> electron;
C     KODE = 1  -  electron <-> proton;
C     KODE = 1  -  proton   <-> proton;
C     KODE = 2  -  electron <-> He+;
C     KODE = 3  -  electron <-> He++;
C     KODE = 4  -  He+      <-> He++.
C---- Calculation from Spitzer (1962), as given in
C     Nowak and Ulmschneider, Astron.Astrophys. Vol 60, 413 (1977).
C---- T  is electron temperature (Kelvins), and
C     P  is electron pressure (dyn/cm**2).
C     !DASH
      save
C     !DASH
      real*8 CROSS, E1, E2, P, RP, T, T2, TR, XL
      integer KODE
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, DIVIDE, HI, BYE
C
      dimension E1(4), E2(4)
C
      data E1 /2.1933D-6, 7.9321D-6, 1.7547D-5, 3.1734D-5/
      data E2 /1.3715D-4, 1.0228D-4, 6.8799D-5, 5.1007D-5/
C
      call HI ('PYTHON')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.4)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1, 2, 3, nor 4.')
        call HALT ('PYTHON',1)
      end if
C
      T2 = T**2
      RP = sqrt(P)
      call DIVIDE (T2,RP,TR)
      XL = log(E2(KODE)*TR)
C
      call DIVIDE ((E1(KODE)*XL),T2,CROSS)
C     !END
      call BYE ('PYTHON')
C
      return
      end
