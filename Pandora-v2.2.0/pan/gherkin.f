      subroutine GHERKIN
     $(SWAVE,SLTIT,NSH,XLM,XLTIT,CALLER)
C
C     Rudolf Loeser, 2002 Aug 21
C---- Saves supplementary headers temporarily.
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, XLM, XLTIT
      integer LUEO, NSH
      logical KILROY
      character CALLER*(*)
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
C     !DASH
C     !EJECT
      external MESHED, MASHED, HI, BYE
C
C               SWAVE(Konwal), SLTIT(Konwal)
      dimension SWAVE(*),      SLTIT(*)
C
      data KILROY /.true./
C
      call HI ('GHERKIN')
C     !BEG
      if(NSH.lt.KONWAL) then
        NSH = NSH+1
        SWAVE(NSH) = XLM
        SLTIT(NSH) = XLTIT
      else
        if(KILROY) then
          KILROY = .false.
          call MESHED ('GHERKIN', 3)
          write (LUEO,100) CALLER,NSH,KONWAL
  100     format(' ','Trouble in GHERKIN, called from ',A,
     $               ': temporary supplementary headers table ',
     $               'is full.'/
     $           ' ','NSH =',I12,', KONWAL =',I12/
     $           ' ','No more will be saved; printed information in ',
     $               'WAVELENGTHS listing will be incomplete.')
          call MASHED ('GHERKIN')
        end if
      end if
C     !END
      call BYE ('GHERKIN')
C
      return
      end
