      subroutine MOLE
     $(TE,V,NDW,AMASS,WVL,DW)
C
C     Rudolf Loeser, 2007 Feb 01
C---- Computes a value of Doppler Width at depth NDW and wavelength
C     WVL (Angstroms), for mass AMASS and using only the simple
C     microturbulent (broadening) velocity V.
C     !DASH
      save
C     !DASH
      real*8 AMASS, DNU, DW, TE, TRM, V, WVL, dummy
      integer NDW
C     !DASH
      external ANGIE, DOPPLER, DIVIDE, HI, BYE
C
C               TE(N), V(N)
      dimension TE(*), V(*)
C
      call HI ('MOLE')
C     !BEG
      call ANGIE   (WVL, DNU)
      call DOPPLER (DNU, TE(NDW), AMASS, (V(NDW)**2), dummy, TRM)
      call DIVIDE  (TRM, DNU, DW)
C     !END
      call BYE ('MOLE')
C
      return
      end
