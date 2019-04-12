      subroutine FULEX
     $(KTYPE,REGCOR,SPECOR,SPEWNG)
C
C     Rudolf Loeser, 2004 Aug 23
C
C---- Looks at continuum wavelength type and tells:
C           REGCOR (regular [constnt bckgr] core) = 1 or 19
C           SPECOR (special [varying bckgr] core) = 18 or 24 or 25 or 26
C           SPEWNG (special [varying bckgr] wing) = 4 or 16
C
C     (This is version 3 of FULEX.)
C     !DASH
      save
C     !DASH
      integer KTYPE
      logical REGCOR, SPECOR, SPEWNG
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
      call HI ('FULEX')
C     !BEG
      if(KTYPE.gt.0) then
        call BEECH (KTYPE)
      end if
C
      REGCOR = KWKS( 1).or.KWKS(19)
      SPECOR = KWKS(18).or.KWKS(24).or.KWKS(25).or.KWKS(26)
      SPEWNG = KWKS( 4).or.KWKS(16)
C     !END
      call BYE ('FULEX')
C
      return
      end
