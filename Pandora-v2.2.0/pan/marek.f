      subroutine MAREK
     $(XLM,XCBL)
C
C     Rudolf Loeser, 1995 Apr 21
C---- Reads that Continuum Block whose wavelength is XLM and that
C     is eligible for the Rates Integrations.
C     (This is version 4 of MAREK.)
C     !DASH
      save
C     !DASH
      real*8 XCBL, XLM
      integer J, JF, JJ, JL, LUEO
      logical OK
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
C     !EJECT
      external LOOKALL, NELUMBO, LETTER, ADONIS, MESHED, LINER, ABORT,
     $         HI, BYE
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      call HI ('MAREK')
C     !BEG
      call LOOKALL     (CONWAV, NUMKON, XLM, WAVEDEL, JF, JL)
      OK = (JL.ge.JF).and.(JL.ne.0)
      if(OK) then
        do 100 J = JF,JL
          call NELUMBO (KONTYP(J), OK)
          if(OK) then
            JJ = J
            goto 101
          end if
  100   continue
C
  101   continue
      end if
C
      if(OK) then
        call LETTER    (KONADR(JJ), XCBL, XLM, 0, 'MAREK')
      end if
C
      if(.not.OK) then
        call MESHED    ('MAREK', 1)
        call ADONIS
        call LINER     (2, LUEO)
        write (LUEO,102) XLM,WAVEDEL,JF,JL
  102   format(' ','Looking for Continuum Block for Rates Integrations',
     $             ': XLM =',1PE24.16,', DELTA =',E10.2/
     $         ' ','JF =',I12,', JL =',I12)
        call ABORT
      end if
C     !END
      call BYE ('MAREK')
C
      return
      end
