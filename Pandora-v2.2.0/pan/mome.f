      subroutine MOME
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Allocates scratch storage for SIAM.
C     (This is version 2 of MOME.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NK
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
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOME')
C     !BEG
      call WGET (IS,  CALLER)
C
      NK = N*NMKUSE
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NMKUSE
      IN( 3) = IN( 2)+NMKUSE
      IN( 4) = IN( 3)+NMKUSE
      IN( 5) = IN( 4)+NMKUSE
      IN( 6) = IN( 5)+MIKLEN
      IN( 7) = IN( 6)+NK
      IN( 8) = IN( 7)+NK
      IN( 9) = IN( 8)+NK
      IN(10) = IN( 9)+NK
      IN(11) = IN(10)+NK
C
      MUX    = IN(11)+NMKUSE
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MOME')
C
      return
      end
