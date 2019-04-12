      subroutine CANADA
     $(NCB,XLCOA,XLCOB,NCOW)
C
C     Rudolf Loeser, 1991 Oct 18
C---- Computes NCOW for TULA.
C     (This is version 4 of CANADA.)
C     !DASH
      save
C     !DASH
      real*8 WHI, WLO, XLCOA, XLCOB
      integer I, IHI, ILO, INCR, IPEX, LUEO, NCB, NCOW
      logical DUMP
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (KZQ( 18),IPEX )
C     !DASH
      external  ZEREN, LINER, MESHED, MASHED, HI, BYE
      intrinsic max
C
C               XLCOA(NCB), XLCOB(NCB)
      dimension XLCOA(*),   XLCOB(*)
C
      call HI ('CANADA')
C     !BEG
      DUMP = (IPEX.lt.0).or.(IPEX.eq.1)
      if(DUMP) then
        call MESHED  ('CANADA', 2)
      end if
C
      do 101 I = 1,NCB
        call ZEREN   (XLCOA(I), ILO, WLO, XLCOB(I), IHI, WHI)
        INCR = max(((IHI-ILO)+1),0)
        NCOW = NCOW+INCR
C
        if(DUMP) then
          call LINER (1, LUEO)
          write (LUEO,100) I,NCB,NUMKON,ILO,IHI,INCR,NCOW,
     $                     XLCOA(I),WLO,WHI,XLCOB(I)
  100     format(' ',6I10,5X,'NCOW=',I10/
     $           ' ',1P4E20.12)
        end if
  101 continue
C
      if(DUMP) then
        call MASHED  ('CANADA')
      end if
C     !END
      call BYE ('CANADA')
C
      return
      end
