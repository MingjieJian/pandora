      subroutine SORGHUM
     $(KRJ,LIJ, DOSNGLE)
C
C     Rudolf Loeser, 1999 Oct 13
C---- Decides whether the single-rate formula should be used for the
C     term "A*Rho".
C     (This is version 6 of SORGHUM.)
C     !DASH
      save
C     !DASH
      integer KRJ, LIJ
      logical DOSNGLE, INJB, INRJ, USNG
C     !DASH
      external HI, BYE
C
      call HI ('SORGHUM')
C     !BEG
      USNG = LIJ.eq.2
      INRJ = KRJ.eq.1
      INJB = KRJ.eq.2
C
      DOSNGLE = INJB .or. (INRJ.and.USNG)
C     !END
      call BYE ('SORGHUM')
C
      return
      end
