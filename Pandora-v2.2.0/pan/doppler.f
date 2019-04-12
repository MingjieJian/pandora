      subroutine DOPPLER
     $(DNU,TE,AMASS,V2,ROOT,DW)
C
C     Rudolf Loeser, 1992 Apr 12
C---- Computes the Doppler width, DW;
C     and
C     also returns separately the value of the square-root term.
C
C---- Given:  DNU   - frequency (10**15 Hz)
C             TE    - temperature (K)
C             AMASS - mass (relative to Hydrogen)
C             V2    - square of velocity ([km/s]**2)
C     !DASH
      save
C     !DASH
      real*8 AMASS, DNU, DW, ROOT, TE, TERM, V2
C     !DASH
      external PARROT, DIVIDE, HI, BYE
C
      call HI ('DOPPLER')
C     !BEG
      call PARROT (TE, AMASS, V2, ROOT, TERM)
      call DIVIDE (TERM, DNU, DW)
C     !END
      call BYE ('DOPPLER')
C
      return
      end
