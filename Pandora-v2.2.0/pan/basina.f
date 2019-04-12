      subroutine BASINA
     $(XLTIT,NSH,SLTIT,LMHD,NH,HEADER)
C
C     Rudolf Loeser, 2002 Aug 16
C---- Makes a complete set of HEADERs describing the current continuum
C     data wavelength, and updates type counters, for BLURT.
C     (This is version 2 of BASINA.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, XLTIT
      integer I, KAK1, KAK2, KAK3, KTYPE, LMHD, NH, NSH
      character HEADER*(*), qummy*10
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
C     .
      equivalence
     $(KAKODS( 1),KAK1 ),(KAKODS( 2),KAK2 ),(KAKODS( 3),KAK3 ),
     $(KAKODS( 4),KTYPE)
C     .
C     !DASH
      external BET, TURKOIS, HI, BYE
C
C               HEADER(LMHD), SLTIT(NSH)
      dimension HEADER(*),    SLTIT(*)
C
      call HI ('BASINA')
C     !BEG
      NH = 1
      call BET         (2, XLTIT)
      call TURKOIS     (KTYPE, KAK1, KAK2, KAK3, HEADER(NH), qummy)
C
      if(NSH.gt.0) then
        do 100 I = 1,NSH
          NH = NH+1
          if(NH.gt.LMHD) then
            goto 101
          end if
C
          call BET     (2, SLTIT(I))
          call TURKOIS (KTYPE, KAK1, KAK2, KAK3, HEADER(NH), qummy)
  100   continue
      end if
C
  101 continue
C     !END
      call BYE ('BASINA')
C
      return
      end
