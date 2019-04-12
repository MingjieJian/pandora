      subroutine IRKEDA
     $(WVNUM,WTAB,INDX,MF,ML,KODE)
C
C     Rudolf Loeser, 2000 Oct 31
C---- Sets up basic data, for SPIDER.
C     (This is version 3 of IRKEDA.)
C     !DASH
      save
C     !DASH
      real*8 WTAB, WVNUM, ZERO
      integer INDX, KODE, MF, ML
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
C     !DASH
C     !EJECT
      external SNUGUD, BOGUMIL, AGATE, HI, BYE
C
C               WVNUM(Numkon), WTAB(Numkon), INDX(Numkon)
      dimension WVNUM(*),      WTAB(*),      INDX(*)
C
      call HI ('IRKEDA')
C     !BEG
      KODE = 0
C
      call SNUGUD  (NUMKON, CONWAV, ZERO, WVNUM, WTAB, 0)
      call BOGUMIL (WTAB, NUMKON, MF, ML)
C
      if((ML-MF).ge.0) then
        call AGATE (NUMKON, INDX, KODE)
      end if
C     !END
      call BYE ('IRKEDA')
C
      return
      end
