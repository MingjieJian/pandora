      subroutine COE
     $(K,KD,ISOTOPE,H)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the function H for FOLD.
C     (This is version 3 of COE.)
C     !DASH
      save
C     !DASH
      real*8 H
      integer ISOTOPE, K, KD
C     !DASH
      external COE12, COE13, HI, BYE
C
      call HI ('COE')
C     !BEG
      if(ISOTOPE.eq.12) then
        call COE12 (K,KD,H)
      else
        call COE13 (K,KD,H)
      end if
C     !END
      call BYE ('COE')
C
      return
      end
