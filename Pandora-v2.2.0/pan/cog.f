      subroutine COG
     $(K,KD,M,ISOTOPE,F)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the function F for FOLD.
C     (This is version 3 of COG.)
C     !DASH
      save
C     !DASH
      real*8 F
      integer ISOTOPE, K, KD, M
C     !DASH
      external COG12, COG13, HI, BYE
C
      call HI ('COG')
C     !BEG
      if(ISOTOPE.eq.12) then
        call COG12 (K,KD,M,F)
      else
        call COG13 (K,KD,M,F)
      end if
C     !END
      call BYE ('COG')
C
      return
      end
