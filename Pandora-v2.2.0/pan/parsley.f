      subroutine PARSLEY
     $(XCBL,N,NF,FREQ,WAVE,VEC,YNT)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Gets data from Continuum Data blocks, for WORM.
C     (This is version 3 of PARSLEY.)
C     !DASH
      save
C     !DASH
      real*8 FREQ, VEC, WAVE, XCBL, YNT
      integer J, KKB, KKCO, KKJNU, N, NF
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
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(13),KKJNU )
      equivalence (KKK(15),KKB   )
C     !DASH
C     !EJECT
      external BEECH, ANGIE, LEYTE, LAGOS, HI, BYE
C
C               WAVE(Numkon), FREQ(Numkon), YNT(N,Numkon), XCBL(Miklen),
      dimension WAVE(*),      FREQ(*),      YNT(N,*),      XCBL(*),
C
C               VEC(N)
     $          VEC(*)
C
      call HI ('PARSLEY')
C     !BEG
      NF = 0
      do 100 J = 1,NUMKON
        call BEECH   (KONTYP(J))
        if(KWKS(13).or.KWKS(15)) then
C         (Use only Composite Lines wavelengths)
          NF       = NF+1
          WAVE(NF) = CONWAV(J)
          call ANGIE (WAVE(NF), FREQ(NF))
          call LEYTE (XCBL, MIKLEN, KONADR(J))
          call LAGOS (N, XCBL(KKCO), XCBL(KKJNU), XCBL(KKB), VEC,
     $                YNT(1,NF))
        end if
  100 continue
C     !END
      call BYE ('PARSLEY')
C
      return
      end
