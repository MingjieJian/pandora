      subroutine BORIS
     $(LTYPE,KAK3,KIND,IU,IL,WAVE,XLTIT)
C
C     Rudolf Loeser, 1992 May 01
C---- Sets up the "effective delta-Lambda index" for VOLVOX / NARKE.
C     !DASH
      save
C     !DASH
      real*8 WAVE, XLTIT
      integer IL, IU, KAK3, KIND, LTYPE, LUEO
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
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external FULEX, MESHED, ABORT, HI, BYE
C
      call HI ('BORIS')
C     !BEG
      call FULEX    (LTYPE, REGCOR, SPECOR, SPEWNG)
C
      if(REGCOR) then
        KIND = 0
C
      else if(SPECOR.or.SPEWNG) then
        KIND = KAK3
C
      else
        call MESHED ('BORIS', 1)
        write (LUEO,100) IU,IL,KAK3,WAVE,XLTIT,LTYPE,KWKS
  100   format(' ','IU =',I12,', IL =',I12,', KAK3 =',I12/
     $         ' ','WAVE =',1PE24.16,', XLTIT =',E24.16,', LTYPE =',I16/
     $         ' ','KWKS =',40L3)
        call ABORT
      end if
C     !END
      call BYE ('BORIS')
C
      return
      end
