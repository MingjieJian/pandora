      subroutine HARTAM
     $(ITYPE,KONTYP)
C
C     Rudolf Loeser, 2002 Aug 20
C---- Sets the bit in KONTYP that corresponds to ITYPE.
C     !DASH
      save
C     !DASH
      integer IBIT, ITYPE, KONTYP
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
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, HI, BYE
      intrinsic mod
C
      call HI ('HARTAM')
C     !BEG
      if((ITYPE.lt.1).or.(ITYPE.gt.NWKS)) then
        write (MSSLIN(1),100) ITYPE,NWKS
  100   format('ITYPE =',I12,', which is not between 1 and',I4,
     $         ', inclusive.')
        call HALT ('HARTAM', 1)
      else
C
        IBIT = 2**(ITYPE-1)
        if(mod((KONTYP/IBIT),2).eq.0) then
          KONTYP = KONTYP+IBIT
        end if
      end if
C     !END
      call BYE ('HARTAM')
C
      return
      end
