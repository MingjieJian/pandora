      subroutine MARABOU
     $(WVLA,WVLM,TEXT)
C
C     Rudolf Loeser, 1985 Jun 03
C---- Sets up a wavelength label, with proper SI units.
C     Input  is WVLA, in Angstroms;
C     output is WVLM, in meters, and
C               TEXT, the encoded character string.
C     !DASH
      save
C     !DASH
      real*8 APM, FAC, WVLA, WVLM
      character TEXT*12
C     !DASH
      external MOGUL, HI, BYE
C
      data APM,FAC /1.D10, 1.0000000000001D0/
C
      call HI ('MARABOU')
C     !BEG
      WVLM = (WVLA/APM)*FAC
      call MOGUL (WVLA, WVLM, TEXT)
C     !END
      call BYE ('MARABOU')
C
      return
      end
