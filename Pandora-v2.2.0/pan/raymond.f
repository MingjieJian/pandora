      subroutine RAYMOND
     $(WAVE,ITYPE,INDEX)
C
C     Rudolf Loeser, 1995 Apr 13
C---- Returns the ordinal (INDEX) in the Continuum Data Blocks index
C     of the block pertaining to wavelength WAVE (in Angstroms) and
C     to type ITYPE. (Does not check for type if ITYPE .le. 0.)
C
C     Returns INDEX = 0 if no such block was found.
C
C     See also LETTER and LAMAR.
C     (This is version 2 of RAYMOND.)
C     !DASH
      save
C     !DASH
      real*8 WAVE
      integer INDEX, ITYPE, J, JF, JL, NORD
      logical TOK, WOK
C     !COM
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
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
C     !EJECT
      external LOOKALL, BEECH, HI, BYE
C
      call HI ('RAYMOND')
C     !BEG
      TOK = .true.
C
      call LOOKALL     (CONWAV, NUMKON, WAVE, WAVEDEL, JF, JL)
      WOK = (JL.ge.JF).and.(JL.ne.0)
C
      if(WOK) then
        NORD = JF
        if(ITYPE.gt.0) then
C
          do 100 J = JF,JL
            call BEECH (KONTYP(J))
            TOK = KWKS(ITYPE)
            if(TOK) then
              NORD = J
              goto 101
            end if
  100     continue
C
  101     continue
        end if
      end if
C
      if(WOK.and.TOK) then
        INDEX = NORD
      else
        INDEX = 0
      end if
C     !END
      call BYE ('RAYMOND')
C
      return
      end
