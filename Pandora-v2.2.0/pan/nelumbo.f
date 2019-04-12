      subroutine NELUMBO
     $(KONTYP,USE)
C
C     Rudolf Loeser, 1995 Apr 21
C---- Determines whether the wavelength corresponding to this value of
C     KONTYP is usable for the Continuum Integrations.
C     (This is version 3 of NELUMBO.)
C     !DASH
      save
C     !DASH
      integer KONTYP
      logical USE
C     !COM
C---- KWACK       as of 2006 Mar 14
      integer     MWKS,NWKS,KSWPR,KSWSH
      parameter   (MWKS=27)
      logical     KWKS
      dimension   KWKS(MWKS),KSWPR(MWKS),KSWSH(MWKS)
C     (Need to revise     TURKOIS     when changing MWKS ! )
      common      /KWACK1/ NWKS,KWKS
      common      /KWACK2/ KSWPR
      common      /KWACK3/ KSWSH
C---- Codes describing "continuum" wavelengths
C
C      1: regular (constant background) line center, printed;
C      2: "additional" wavelength, no Eclipse;
C      3: "additional" wavelength, with Eclipse;
C      4: line source function background, PRD;
C      5: rates integrations, regular;
C      6: additional photoionization;
C      7: H- calculation;
C      8: dust temperature adjustment procedure;
C      9: HSE calculation;
C     10: "Lyman" calculation (level-K-to-continuum integration);
C     11: incident coronal radiation;
C     12: rates integrations, K-shell;
C     13: composite line opacity, no Eclipse;
C     14: miscellaneous;
C     15: composite line opacity, with Eclipse;
C     16: line source function background, FDB;
C     17: actual CO-lines opacity, fundamental;
C     18: FDB line center, printed;
C     19: regular (constant background) line center, not printed;
C     20: actual CO-lines opacity, first overtone;
C     21: actual CO-lines opacity, band limit;
C     22: actual CO-lines opacity, rotational;
C     23: actual CO-lines opacity, second overtone.
C     24: PRD line center, printed;
C     25: PRD line center, not printed;
C     26: FDB line center, not printed;
C     27: standard background.
C     .
C     !DASH
C     !EJECT
      external BEECH, HI, BYE
C
      call HI ('NELUMBO')
C     !BEG
      call BEECH (KONTYP)
      USE = KWKS( 5) .or. KWKS( 6) .or. KWKS( 7) .or. KWKS( 8) .or.
     $      KWKS(12) .or. KWKS(13) .or. KWKS(15) .or. KWKS(27)
C     !END
      call BYE ('NELUMBO')
C
      return
      end
