      subroutine BLISS
     $(N,XMBJL,RHMFF,XHBJL,RHFF,INDW)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Accumulates integrals, for SALINA.
C     (This is version 3 of BLISS.)
C     !DASH
      save
C     !DASH
      real*8 FAC, FOUR, PI, RHFF, RHMFF, XHBJL, XMBJL, Z
      integer INDW, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 5),FOUR  )
C     !DASH
      external TWACK, ARRINC, HI, BYE
C
C               XMBJL(N), RHMFF(N), XHBJL(N), RHFF(N)
      dimension XMBJL(*), RHMFF(*), XHBJL(*), RHFF(*)
C
C
      call HI ('BLISS')
C     !BEG
      call TWACK  (INDW, NUMKON, CONWAV, Z)
      FAC = FOUR*PI*Z
C
      call ARRINC (XMBJL, FAC, RHMFF, N)
      call ARRINC (XHBJL, FAC, RHFF,  N)
C     !END
      call BYE ('BLISS')
C
      return
      end
