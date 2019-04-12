      subroutine DEREK
C
C     Rudolf Loeser, 1993 Feb 25
C---- Prints wavelength types summary.
C     !DASH
      save
C     !DASH
      integer J, NO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !EJECT
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
      external ABJECT, LINER, HI, BYE
C
      call HI ('DEREK')
C     !BEG
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Summary of Continuum Calculations wavelengths ',
     $           'by type.')
      call LINER  (2, NO)
C
      write (NO,102) (KSWPR(J),KSWSH(J),J=1,15)
  102 format(' ',I8,' (',I6,')',2X,'line center, regular, printed'/
     $       ' ',I8,' (',I6,')',2X,'"additional" wavelength, ',
     $                            'no eclipse'/
     $       ' ',I8,' (',I6,')',2X,'"additional" wavelength, ',
     $                            'with eclipse'/
     $       ' ',I8,' (',I6,')',2X,'line background, PRD'/
     $       ' ',I8,' (',I6,')',2X,'rates integration, regular'/
     $       ' ',I8,' (',I6,')',2X,'additional photoionization'/
     $       ' ',I8,' (',I6,')',2X,'H- calculation'/
     $       ' ',I8,' (',I6,')',2X,'dust temperature adjustment'/
     $       ' ',I8,' (',I6,')',2X,'HSE calculation'/
     $       ' ',I8,' (',I6,')',2X,'"Lyman" calculation'/
     $       ' ',I8,' (',I6,')',2X,'incident coronal radiation'/
     $       ' ',I8,' (',I6,')',2X,'rates integration, K-shell'/
     $       ' ',I8,' (',I6,')',2X,'composite line opacity, ',
     $                            'no eclipse'/
     $       ' ',I8,' (',I6,')',2X,'miscellaneous'/
     $       ' ',I8,' (',I6,')',2X,'composite line opacity, ',
     $                            'with eclipse')
      write (NO,103) (KSWPR(J),KSWSH(J),J=16,NWKS)
  103 format(' ',I8,' (',I6,')',2X,'line background, FDB'/
     $       ' ',I8,' (',I6,')',2X,'actual CO-lines, fundamental'/
     $       ' ',I8,' (',I6,')',2X,'line center, FDB, printed'/
     $       ' ',I8,' (',I6,')',2X,'line center, regular, not printed'/
     $       ' ',I8,' (',I6,')',2X,'actual CO-lines, first overtone'/
     $       ' ',I8,' (',I6,')',2X,'actual CO-lines, band limit'/
     $       ' ',I8,' (',I6,')',2X,'actual CO-lines, rotational'/
     $       ' ',I8,' (',I6,')',2X,'actual CO-lines, second overtone'/
     $       ' ',I8,' (',I6,')',2X,'line center, PRD, printed'/
     $       ' ',I8,' (',I6,')',2X,'line center, PRD, not printed'/
     $       ' ',I8,' (',I6,')',2X,'line center, FDB, not printed'/
     $       ' ',I8,' (',I6,')',2X,'standard background')
      call LINER  (1, NO)
      write (NO,104) NUMKON,KONWAL
  104 format(' ',I8,11X,'TOTAL number of wavelength values'/
     $       ' ',I8,11X,'LIMIT        of wavelength values (built-in)')
      call LINER  (1, NO)
      write (NO,105) WAVEDEL
  105 format(' ',1PE8.1,11X,'= WAVEDEL, the relative difference ',
     $           'criterion for distinct wavelengths (built-in)')
C     !END
      call BYE ('DEREK')
C
      return
      end
