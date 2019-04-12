      subroutine JACOB
     $(KODE,MODE,NW,WAVES,IADRS,KTYPE)
C
C     Rudolf Loeser, 2004 Aug 19
C---- Sets up the subset of continuum wavelengths specified by KODE.
C
C     KODE = 1: "General"
C     KODE = 2: LYMAN
C     KODE = 3: current line transition (as told in LINUS)
C
C     MODE selects line wavelength type and is used only if KODE = 3:
C          MODE = 1: use line core [constnt bckgr] wavelength only
C          MODE = 2: use FDB or PRD [varying bckgr] line wavelengths
C          MODE = 3: use all types of line wavelengths
C
C     (This is version 2 of JACOB.)
C     !DASH
      save
C     !DASH
      real*8 WAVES
      integer I, IADRS, KODE, KTYPE, MODE, NW
      logical YES
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
C     !DASH
C     !EJECT
      external POOLE, HI, BYE
C
C               NWMAX = NUMKON or KM
C
C               WAVES(NWMAX), IADRS(NWMAX), KTYPE(NWMAX)
      dimension WAVES(*),     IADRS(*),     KTYPE(*)
C
      call HI ('JACOB')
C     !BEG
      NW = 0
      do 100 I = 1,NUMKON
        call POOLE (I, KODE, MODE, YES)
        if(YES) then
          NW        = NW+1
          WAVES(NW) = CONWAV(I)
          IADRS(NW) = KONADR(I)
          KTYPE(NW) = KONTYP(I)
        end if
  100 continue
C     !END
      call BYE ('JACOB')
C
      return
      end
