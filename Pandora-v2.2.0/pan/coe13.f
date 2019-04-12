      subroutine COE13
     $(K,KD,H)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the function H, for the Carbon-13 isotope, for COE.
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
      external  HI, BYE
C
      dimension CH(10,2)
C
      data CH / 1.0000D+00, -1.3884D-03, -1.7167D-04, -1.2586D-06,
     $          9.7746D-08, -1.7236D-08,  1.2038D-09, -4.9605D-11,
     $          1.0749D-12, -9.4714D-15,
C
     $          9.9999D-01,  2.2797D-02, -2.0382D-04,  5.6798D-05,
     $         -9.3750D-06,  8.7786D-07, -4.8949D-08,  1.6259D-09,
     $         -2.9579D-11,  2.2651D-13/
C
      call HI ('COE13')
C     !BEG
      H  = CH(1,KD)
      XK = K
      PK = ONE
      do 100 I = 2,10
        PK = PK*XK
        H  = H+CH(I,KD)*PK
  100 continue
C     !END
      call BYE ('COE13')
C
      return
      end
