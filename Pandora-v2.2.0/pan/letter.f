      subroutine LETTER
     $(IADRS,BLOCK,XLM,ITYPE,CALLER)
C
C     Rudolf Loeser, 1995 Apr 12
C---- Returns the Continuum Data Block at record address IADRS, and
C     checks whether it pertains to wavelength XLM and type ITYPE
C     (does not check type if ITYPE .le. 0).
C
C     Aborts with error printout if unsuccessful.
C
C     See also RAYMOND and LAMAR.
C     (This is version 3 of LETTER.)
C     !DASH
      save
C     !DASH
      real*8 BLOCK, ONE, WAVE, XLM, XLTIT
      integer IADRS, IFLG, ITYPE, KKLAMD, KKLTIT, KTYPE, LUEO
      logical OK, TYPEOK, WAVEOK
      character CALLER*(*)
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
C     !EJECT
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK( 1),KKLTIT)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 4),KTYPE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LEYTE, COMPD, BET, BEECH, LINER, ADONIS, MESHED, ABORT,
     $         HI, BYE
C
C               BLOCK(Miklen)
      dimension BLOCK(*)
C
      call HI ('LETTER')
C     !BEG
C---- Read record
      BLOCK(KKLAMD) = -ONE
      BLOCK(KKLTIT) = -ONE
C
      call LEYTE    (BLOCK, MIKLEN, IADRS)
C
      WAVE  = BLOCK(KKLAMD)
      XLTIT = BLOCK(KKLTIT)
C
C---- Check wavelength
      call COMPD    (WAVE, XLM, WAVEDEL, IFLG)
      WAVEOK = IFLG.eq.0
C
      TYPEOK = .true.
      if(ITYPE.gt.0) then
C----   Check type
        call BET    (2, XLTIT)
        call BEECH  (KTYPE)
        TYPEOK = KWKS(ITYPE)
      end if
C
      OK = WAVEOK.and.TYPEOK
C
      if(.not.OK) then
C----   Write error message
        call MESHED ('LETTER', 1)
        call ADONIS
        call LINER  (2, LUEO)
        write (LUEO,100) IADRS, WAVE,XLM,ITYPE,CALLER
  100   format(' ','Trouble reading Continuum Block at address =',I10/
     $         ' ','WAVE =',1PE24.14,5X,'desired XLM =',E24.14,5X,
     $             'ITYPE =',I3//
     $         ' ','Called from: ',A)
        call ABORT
      end if
C     !END
      call BYE ('LETTER')
C
      return
      end
