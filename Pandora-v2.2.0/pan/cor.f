      subroutine COR
     $(K,M,ISOTOPE,G)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the function G for FOLD.
C     (This is version 2 of COR.)
C     !DASH
      save
C     !DASH
      real*8 G
      integer ISOTOPE, K, M
C     !DASH
      external COR12, COR13, HI, BYE
C
      call HI ('COR')
C     !BEG
      if(ISOTOPE.eq.12) then
        call COR12  (K,M,G)
      else
        call COR13  (K,M,G)
      end if
C     !END
      call BYE ('COR')
C
      return
      end
