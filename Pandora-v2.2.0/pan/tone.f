      subroutine TONE
     $(W,IW)
C
C     Rudolf Loeser, 1984 Feb 03
C---- Sorts the Continuum Data Block index.
C     (This is version 3 of TONE.)
C     !DASH
      save
C     !DASH
      real*8 W
      integer IN, IPNT, IS, IVC1, IVC2, IW, IWS, JN, MOX, MUX
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
      external MARGO, SORT, ORDERI, ORDERD, JULIET, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IVC1  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),IPNT  ),(JN( 2),IVC2  )
C
      call HI ('TONE')
C     !BEG
      if(NUMKON.gt.1) then
C       (Get, and allocate, W & IW allotments)
        call JULIET (IN, IS , MOX, 'TONE')
        call MARGO  (JN, IWS, MUX, 'TONE')
C
        call SORT   (CONWAV, NUMKON, IW(IPNT), 'Continuum wavelengths')
C
        call ORDERD (CONTIT, IW(IPNT), NUMKON, W (IVC1))
        call ORDERI (KONADR, IW(IPNT), NUMKON, IW(IVC2))
        call ORDERI (KONTYP, IW(IPNT), NUMKON, IW(IVC2))
        call ORDERI (KONLIC, IW(IPNT), NUMKON, IW(IVC2))
        call ORDERI (KONNSH, IW(IPNT), NUMKON, IW(IVC2))
C
C       (Give back W & IW allotments)
        call WGIVE  (W , 'TONE')
        call IGIVE  (IW, 'TONE')
      end if
C     !END
      call BYE ('TONE')
C
      return
      end
