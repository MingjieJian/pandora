      subroutine LEX
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1971 Oct 29
C---- Allocates scratch storage for PUELLA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NNUM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LEX')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNUM = N*NMKUSE
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NMKUSE
      IN( 3) = IN( 2)+NMKUSE
      IN( 4) = IN( 3)+NMKUSE
      IN( 5) = IN( 4)+NNUM
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NNUM
      IN( 9) = IN( 8)+NNUM
      IN(10) = IN( 9)+NNUM
      MUX    = IN(10)+NNUM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LEX')
C
      return
      end
