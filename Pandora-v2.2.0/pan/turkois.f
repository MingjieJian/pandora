      subroutine TURKOIS
     $(KTYPE,K1,K2,K3,HEADER,BRIEF)
C
C     Rudolf Loeser, 2000 Aug 14
C---- Makes up a narrative line describing a Continuum Data Block.
C     (This is version 3 of TURKOIS.)
C     !DASH
      save
C     !DASH
      integer K1, K2, K3, KTYPE
      character BLANK*1, BRIEF*10, HEADER*(*), LAB*9
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external LOBELIA, ABELLIO, HALT, HI, BYE
C
      dimension LAB(2)
C
      data LAB /'radiative', 'passive'/
C
      call HI ('TURKOIS')
C     !BEG
      if((KTYPE.lt.1).or.(KTYPE.gt.NWKS)) then
        write (MSSLIN(1),100) KTYPE,NWKS
  100   format('KTYPE =',I12,', NWKS =',I12,': KTYPE out of range.')
        call HALT ('TURKOIS', 1)
      end if
C
      HEADER = BLANK
      BRIEF  = BLANK
C
      goto (
     $ 101, 102, 102, 104, 105,  106, 107, 108, 999, 110,
     $ 111, 105, 113, 999, 113,  116, 117, 116, 101, 117,
     $ 117, 117, 117, 104, 104,  116, 105
     $ ), KTYPE
C     !EJECT
  101 continue
        write (HEADER,201) K2,K3,LAB(K1)
  201   format('Line ',I2,'/',I2,', ',A9)
        write (BRIEF,301) K2,K3
  301   format('Core ',I2,'/',I2)
        goto 999
  102 continue
        HEADER = 'Additional wavelength'
        write (BRIEF,302) K1
  302   format('Add',I7)
        goto 999
  104 continue
        write (HEADER,204) K2,K3,K1
  204   format('P.R.D. ',I2,'/',I2,' #',I7)
        write (BRIEF,304) K2,K3,K1
  304   format(I2,'/',I2,'!',I4)
        goto 999
  105 continue
        call ABELLIO (HEADER, BRIEF)
        goto 999
  106 continue
        write (HEADER,206) K2,K1
  206   format('Add. Photoion., Lvl ',I2,' #',I7)
        write (BRIEF,306) K2,K1
  306   format('AP',I2,'!',I5)
        goto 999
  107 continue
        write (HEADER,207) K1
  207   format('H- Continuum, #',I7)
        write (BRIEF,307) K1
  307   format('H-!',I7)
        goto 999
  108 continue
        write (HEADER,208) K1
  208   format('Dust, #',I7)
        write (BRIEF,308) K1
  308   format('Dust!',I5)
        goto 999
C     !EJECT
  110 continue
        write (HEADER,210) K1
  210   format('"Lyman" continuum, #',I7)
        write (BRIEF,310) K1
  310   format('Ly!',I7)
        goto 999
  111 continue
        write (HEADER,211) K1
  211   format('Coronal Radiation, #',I7)
        write (BRIEF,311) K1
  311   format('CR!',I7)
        goto 999
  113 continue
        write (HEADER,213) K1
  213   format('Composite Line Opacity, #',I7)
        write (BRIEF,313) K2,K1
  313   format(I1,'KM',I7)
        goto 999
  116 continue
        write (HEADER,216) K2,K3,K1
  216   format('Line ',I2,'/',I2,' background, #',I7)
        write (BRIEF,316) K2,K3,K1
  316   format(I2,'/',I2,'!',I4)
        goto 999
  117 continue
C       CO lines label
        call LOBELIA (HEADER, BRIEF)
        goto 999
C
  999 continue
C     !END
      call BYE ('TURKOIS')
C
      return
      end
