      subroutine MARTHA
     $(XLM,ITYPE, EXISTS)
C
C     Rudolf Loeser, 1995 Apr 06
C---- MARTHA enables the sharing of Continuum Blocks that arise
C     in different contexts ( ITYPE ! ).
C
C     If XLM equals the wavelength of one of the Blocks already
C     allocated, then EXISTS is returned = .true., and
C     KONTYP for that Block is updated (by making sure that the
C     bit corresponding to ITYPE is set);
C     if not, the EXISTS is returned = .false., and MARTHA does
C     nothing else.
C
C     Note: MARTHA is only called in cases when such sharing is
C     appropriate.
C
C     (This is version 5 of MARTHA.)
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer ITYPE, K, LOOK
      logical EXISTS
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
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external  LOOKUD, HARTAM, HI, BYE
C
      call HI ('MARTHA')
C     !BEG
      EXISTS = .false.
C
      if(NUMKON.gt.0) then
        call LOOKUD   (CONWAV, NUMKON, WAVEDEL, XLM, K, LOOK)
        EXISTS = LOOK.eq.1
C
        if(EXISTS) then
C----     Make sure ITYPE is recorded in KONTYP
          call HARTAM (ITYPE, KONTYP(K))
        end if
      end if
C     !END
      call BYE ('MARTHA')
C
      return
      end
