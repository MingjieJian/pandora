      subroutine MARION
     $(BACKGR,LINCON)
C
C     Rudolf Loeser, 2003 Mar 26
C---- Sets up NMKUSE, the wavelength count to be used for storage
C     allocation in SIAM.
C     (This is version 2 of MARION.)
C     !DASH
      save
C     !DASH
      integer KM, LUEO
      logical BACKGR, KILROY, LINCON
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MORTON, MESHED, ABORT, HI, BYE
C
      data KILROY /.true./
C
      call HI ('MARION')
C     !BEG
      if(KILROY) then
        KILROY = .false.
C----   Count the number of line-specific wavelengths
        call MORTON (NUMKON, CONTIT, NUMTRU)
      end if
C
      NMKUSE = -1
      if(BACKGR) then
        NMKUSE = NUMKON-NUMTRU
        goto 100
      end if
      if(LINCON) then
        NMKUSE = KM
      end if
  100 continue
C
      if(NMKUSE.lt.0) then
        call MESHED ('MARION',1)
        write (LUEO,101) BACKGR,LINCON,NUMKON,NUMTRU,KM,NMKUSE
  101   format(' ','Trouble computing NMKUSE.',10X,'BACKGR =',L12,
     $             ', LINCON =',L12//
     $         ' ','NUMKON =',I12,', NUMTRU =',I12,', KM =',I12,
     $             ', NMKUSE =',I12)
        call ABORT
      end if
C     !END
      call BYE ('MARION')
C
      return
      end
