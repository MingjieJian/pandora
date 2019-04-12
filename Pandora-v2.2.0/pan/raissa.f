      subroutine RAISSA
     $(XCBL,Z,TE)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Produces a Continuum Data Save File.
C     (This is version 3 of RAISSA.)
C     !DASH
      save
C     !DASH
      real*8 TE, XCBL, Z
      integer I, LS
C     !COM
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
      external GERIN, LEYTE, HI, BYE
C
C               Z(N), TE(N), XCBL(Miklen)
      dimension Z(*), TE(*), XCBL(MIKLEN)
C
      call HI ('RAISSA')
C     !BEG
      call GERIN      (LS, NUMKON, Z, TE)
C
      if(LS.gt.0) then
        do 100 I = 1,NUMKON
C
          call LEYTE  (XCBL, MIKLEN, KONADR(I))
C
          write (LS) XCBL
C
  100   continue
      end if
C     !END
      call BYE ('RAISSA')
C
      return
      end
