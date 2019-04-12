      subroutine MORTON
     $(NW,CONTIT,NUMTRU)
C
C     Rudolf Loeser, 2003 Mar 26
C---- Counts wavelengths by primary type, and
C     computes NUMTRU = # of line-specific wavelengths.
C     (This is version 2 of MORTON.)
C     !DASH
      save
C     !DASH
      real*8 CONTIT
      integer I, KTYPE, NUMTRU, NW
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
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 4),KTYPE)
C     !DASH
      external ZEROI, BET, HI, BYE
C
C               CONTIT(NW)
      dimension CONTIT(*)
C
      call HI ('MORTON')
C     !BEG
      call ZEROI (KSWPR, 1, NWKS)
      do 100 I = 1,NW
        call BET (2, CONTIT(I))
        KSWPR(KTYPE) = KSWPR(KTYPE)+1
  100 continue
C
      NUMTRU = KSWPR( 1)+KSWPR( 4)+KSWPR(16)+KSWPR(18)+
     $         KSWPR(19)+KSWPR(24)+KSWPR(25)+KSWPR(26)
C     !END
      call BYE ('MORTON')
C
      return
      end
