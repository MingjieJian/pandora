      subroutine EEL
     $(IPACK,I1,I2,I3,I4)
C
C     Rudolf Loeser, 1984 Jun 21
C---- Unpacks IPACK: control switches for detail output from continuum
C     flux calculation.
C     (This is version 3 of EEL.)
C     !DASH
      save
C     !DASH
      integer I1, I2, I3, I4, IPACK, J
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      call HI ('EEL')
C     !BEG
      J  = IPACK
      I1 = mod(J,2)
      J  = J/2
      I2 = mod(J,2)
      J  = J/2
      I3 = mod(J,2)
      I4 = J/2
C     !END
      call BYE ('EEL')
C
      return
      end
