      subroutine HYDATA
     $(NPQ,XNU,WN,XLAM)
C
C     Rudolf Loeser, 1998 Jan 21
C---- Computes Hydrogen levels data.
C
C     Input : NPQ  is the principal quantum number (use 0 for infinity)
C
C     Output: XNU  is the continuum edge frequency (10**15 Hz)
C             WN   is the continuum edge wavenumber
C             XLAM is the continuum edge wavelength (Angstroms).
C     !DASH
      save
C     !DASH
      real*8 AML, ANGPCM, CLIGHT, FRQUNT, ONE, PN2, PN4, PQN, REL, RYH,
     $       SUM, WN, XLAM, XNU, ZERO
      integer NPQ
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 3),CLIGHT)
      equivalence (TUNI( 3),FRQUNT)
      equivalence (TUNI( 6),ANGPCM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      data RYH,REL,AML /1.096775815D5, 1.4601D0, 2.7095D-1/
C     !EJECT
C
      call HI ('HYDATA')
C     !BEG
      SUM = RYH+(REL-AML)
      if(NPQ.le.0) then
C
        WN   = SUM
        XLAM = ZERO
C
      else if(NPQ.eq.1) then
C
        WN   = ZERO
        XLAM = ANGPCM/SUM
C
      else
        PQN  = NPQ
        PN2  = PQN**2
        PN4  = PN2**2
C
        WN   = RYH*(ONE-ONE/PN2)+REL*(ONE-ONE/PN4)-AML
        XLAM = (PN2*ANGPCM)/(RYH+REL/PN2)
      end if
C
      XNU = (WN*CLIGHT)/FRQUNT
C     !END
      call BYE ('HYDATA')
C
      return
      end
