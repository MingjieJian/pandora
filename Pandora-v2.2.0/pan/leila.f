      subroutine LEILA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 10
C---- Allocates scratch storage for RINA.
C     !DASH
      save
C     !DASH
      integer IN, IS, KSHEL, LIM, MMR, MMRP, MOX, MRX, MRXP, MUX, N,
     $        NMRXP, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(18),MMR)
      equivalence (JZQ(30),MRX)
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 1),KSHEL)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     !EJECT
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
      external  WGET, WLCK, HI, BYE
      intrinsic min
C
      dimension IN(*)
C
      call HI ('LEILA')
C     !BEG
      call WGET (IS,  CALLER)
C
      LIM   = NSL+min(KSHEL,1)
      MMRP  = MMR+1
      MRXP  = MRX+1
      NMRXP = N*MRXP
      MOX   = N*LIM
C     !EJECT
      IN( 1) = IS
C
      IN( 2) = IN( 1)+MRXP
      IN( 3) = IN( 2)+MRXP
      IN( 4) = IN( 3)+NMRXP
      IN( 5) = IN( 4)+NMRXP
      IN( 6) = IN( 5)+NMRXP
      IN( 7) = IN( 6)+NMRXP
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+MMRP
      IN(11) = IN(10)+MRXP
C
      IN(12) = IN(11)+MRXP
      IN(13) = IN(12)+MOX
      IN(14) = IN(13)+MOX
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+NUMKON
      IN(20) = IN(19)+N
      IN(21) = IN(20)+N
C
      IN(22) = IN(21)+N
      IN(23) = IN(22)+N
      IN(24) = IN(23)+MOX
      MUX    = IN(24)+MIKLEN
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LEILA')
C
      return
      end
