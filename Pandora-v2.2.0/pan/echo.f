      subroutine ECHO
     $(KONTYP,LINE,N,LIMIT)
C
C     Rudolf Loeser, 1995 Apr 14
C---- Makes a printable list of types, for ADONIS.
C     (This is version 4 of ECHO.)
C     !DASH
      save
C     !DASH
      integer I, KONTYP, LIM, LIMIT, N
      character LINE*(*)
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
      call HI ('ECHO')
C     !BEG
      call BEECH (KONTYP)
C
      N   = 0
      LIM = LIMIT-3
      do 102 I = 1,NWKS
C
        if(KWKS(I)) then
          if(I.le.9) then
            write (LINE(N+1:N+2),100) I
  100       format(I2)
            N = N+2
          else
            write (LINE(N+1:N+3),101) I
  101       format(I3)
            N = N+3
          end if
          if(N.gt.LIM) goto 103
        end if
C
  102 continue
C
  103 continue
C     !END
      call BYE ('ECHO')
C
      return
      end
