      subroutine ROMNEY
     $(TE,BETA)
C
C     Rudolf Loeser, 1983 Aug 16
C---- Exhibits extrema of exp[-(h*nu)/(k*T)].
C     !DASH
      save
C     !DASH
      real*8 BETA, DELTA, TE, WAVMAX, WAVMIN
      integer IFLG, MAMAS, N, NO
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
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 73),MAMAS)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external ABJECT, COMPD, RICCALL, HI, BYE
C
C               TE(N), BETA(N)
      dimension TE(*), BETA(*)
C
      data DELTA /1.D-2/
C
      call HI ('ROMNEY')
C     !BEG
      if(MAMAS.gt.0) then
        call ABJECT    (NO)
        write (NO,100)
  100   format(' ','Sample tables of exp -( h*nu/k*T ).')
C
        WAVMIN = CONWAV(     1)
        WAVMAX = CONWAV(NUMKON)
        call RICCALL   (N, TE, BETA, NO, WAVMIN)
C
        call COMPD     (WAVMIN, WAVMAX, DELTA, IFLG)
        if(IFLG.ne.0) then
          call RICCALL (N, TE, BETA, NO, WAVMAX)
        end if
      end if
C     !END
      call BYE ('ROMNEY')
C
      return
      end
