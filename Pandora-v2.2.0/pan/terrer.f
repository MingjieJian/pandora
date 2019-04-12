      subroutine TERRER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1996 Feb 27
C---- Allocates scratch storage for WAGRIN.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N5, NB
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
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
C     !EJECT
C---- WAGRAM      as of 1997 Nov 19
      real*8      XWGRTC
      integer     IWGRBS
      common      /WAGRAM1/ IWGRBS
      common      /WAGRAM2/ XWGRTC
C     Control parameters for scattering albedo analysis (LINECOMP):
C     IWGRBS - size of wavelength batch, from which 2 are picked,
C     XWGRTC - minimum TAU criterion.
C     .
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('TERRER')
C     !BEG
      call WGET (IS,  CALLER)
C
      N5 = 5*N
      NB = IWGRBS*N
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NUMKON
      IN( 3) = IN( 2)+N5
      IN( 4) = IN( 3)+N5
      IN( 5) = IN( 4)+IWGRBS
      IN( 6) = IN( 5)+MIKLEN
      IN( 7) = IN( 6)+NB
      IN( 8) = IN( 7)+NB
      IN( 9) = IN( 8)+NB
      MUX    = IN( 9)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('TERRER')
C
      return
      end
