      subroutine ZEREN
     $(XLMA,ILO,WLO,XLMB,IHI,WHI)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Gets limiting wavelengths and their indices, for some
C     cooling rates calculations.
C
C     It is assumed here that if there are in the index several values
C     of wavelength "equal to" XLMA or XLMB, then it does not matter
C     which ones are selected.
C     !DASH
      save
C     !DASH
      real*8 WHI, WLO, XLMA, XLMB
      integer IHI, ILO
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
      external NOTMORE, NOTLESS, HI, BYE
C
      call HI ('ZEREN')
C     !BEG
      call NOTLESS (CONWAV, NUMKON, XLMA, ILO)
C
      if((ILO.le.0).or.(ILO.gt.NUMKON)) then
        ILO = 1
      end if
      WLO = CONWAV(ILO)
C
      call NOTMORE (CONWAV, NUMKON, XLMB, IHI)
C
      if((IHI.le.0).or.(IHI.gt.NUMKON)) then
        IHI = NUMKON
      end if
      WHI = CONWAV(IHI)
C     !END
      call BYE ('ZEREN')
C
      return
      end
