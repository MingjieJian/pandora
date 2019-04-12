      subroutine BELISAR
     $(NSH)
C
C     Rudolf Loeser, 2002 Aug 20
C---- Initializes Continuum Blocks index and other data.
C     !DASH
      save
C     !DASH
      integer NSH
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
      external ZERO1, ZEROI, HI, BYE
C
      call HI ('BELISAR')
C     !BEG
      call ZERO1 (CONTIT,    KONWAL)
      call ZERO1 (CONWAV,    KONWAL)
      call ZEROI (KONADR, 1, KONWAL)
      call ZEROI (KONLIC, 1, KONWAL)
      call ZEROI (KONTYP, 1, KONWAL)
      call ZEROI (KONNSH, 1, KONWAL)
      NSH = 0
C     !END
      call BYE ('BELISAR')
C
      return
      end
