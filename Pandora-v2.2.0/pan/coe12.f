      subroutine COE12
     $(K,KD,H)
C
C     Rudolf Loeser, 1992 Sep 11
C---- Computes the function H, for the Carbon-12 isotope, for COE.
C     !DASH
      save
C     !DASH
      real*8 CH, H, ONE, PK, XK
      integer I, K, KD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      dimension CH(10,2)
C
      data CH / 1.0000D+00, -1.4243D-03, -1.8776D-04,  2.5099D-06,
     $         -7.0693D-07,  7.1219D-08, -4.3129D-09,  1.4553D-10,
     $         -2.5841D-12,  1.8832D-14,
C
     $          1.0000D+00,  2.3035D-02,  1.5977D-05, -1.5246D-05,
     $          2.7701D-06, -3.0533D-07,  2.0163D-08, -7.5946D-10,
     $          1.5284D-11, -1.2803D-13/
C
      call HI ('COE12')
C     !BEG
      H  = CH(1,KD)
      XK = K
      PK = ONE
      do 100 I = 2,10
        PK = PK*XK
        H  = H+CH(I,KD)*PK
  100 continue
C     !END
      call BYE ('COE12')
C
      return
      end
