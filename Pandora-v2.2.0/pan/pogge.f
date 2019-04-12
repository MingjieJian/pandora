      subroutine POGGE
     $(OPAC,WAVE,LTYPE,NW,N,NF,WAF,OP)
C
C     Rudolf Loeser, 1986 Mar 17
C---- Gets subsets of WAVE and OPAC, for BRILL.
C     !DASH
      save
C     !DASH
      real*8 OP, OPAC, WAF, WAVE
      integer I, LTYPE, N, NF, NW
      logical JUNK, LINE, REGCOR, SPECOR, SPEWNG
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
      external FULEX, MOVE1, HI, BYE
C
C               WAVE(NW), WAF(NW), OP(N,NW), OPAC(N,NW), LTYPE(NW)
      dimension WAVE(*),  WAF(*),  OP(N,*),  OPAC(N,*),  LTYPE(*)
C
      call HI ('POGGE')
C     !BEG
      NF = 0
      do 100 I = 1,NW
        call FULEX   (LTYPE(I), REGCOR, SPECOR, SPEWNG)
        LINE = REGCOR.or.SPECOR.or.SPEWNG
        JUNK = KWKS( 9).or.KWKS(14)
C
        if(.not.(LINE.or.JUNK)) then
          NF      = NF+1
          WAF(NF) = WAVE(I)
          call MOVE1 (OPAC(1,I), N, OP(1,NF))
        end if
C
  100 continue
C     !END
      call BYE ('POGGE')
C
      return
      end
