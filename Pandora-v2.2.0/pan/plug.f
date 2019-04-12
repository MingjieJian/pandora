      subroutine PLUG
     $(XLM,KTAB,NT,XCBL)
C
C     Rudolf Loeser, 2002 Oct 15
C---- Tries to read a Continuum Data block for XLM, provided it has
C     one of the ITYPEs listed (in priority order) in KTAB.
C
C     Returns with the contents in XCBL if successful,
C     aborts otherwise.
C     !DASH
      save
C     !DASH
      real*8 XCBL, XLM
      integer I, INDEX, KTAB, LUEO, NT
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
C     !DASH
C     !EJECT
      external RAYMOND, LETTER, MESHED, ABORT, HI, BYE
C
C               XCBL(Miklen), KTAB(NT)
      dimension XCBL(*),      KTAB(*)
C
      call HI ('PLUG')
C     !BEG
      do 100 I = 1,NT
        call RAYMOND (XLM, KTAB(I), INDEX)
        if(INDEX.gt.0) then
          goto 104
        end if
  100 continue
C
      call MESHED    ('PLUG', 1)
      write (LUEO,101)
  101 format(' ','List of ITYPEs')
      write (LUEO,102) (KTAB(I),I=1,NT)
  102 format(' ',7X,12I10)
      write (LUEO,103) XLM
  103 format(' ','XLM =',1PE24.16,': not found with one of the ITYPEs ',
     $           'specified.')
      call ABORT
C
  104 continue
      call LETTER    (KONADR(INDEX), XCBL, XLM, 0, 'PLUG')
C     !END
      call BYE ('PLUG')
C
      return
      end
