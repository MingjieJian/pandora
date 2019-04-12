      subroutine LAMAR
     $(WAVE,ITYPE,BLOCK)
C
C     Rudolf Loeser, 1995 Apr 13
C---- Attempts to locate and read the Continuum Data Block pertaining
C     to wavelength WAVE and type ITYPE (ignores type if ITYPE .le. 0).
C     Returns with the contents in BLOCK if successful;
C     aborts the run if not.
C     (This is version 2 of LAMAR.)
C     !DASH
      save
C     !DASH
      real*8 BLOCK, WAVE
      integer INDEX, ITYPE
      logical OK
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external RAYMOND, LETTER, HALT, HI, BYE
C
C               BLOCK(Miklen)
      dimension BLOCK(*)
C
      call HI ('LAMAR')
C     !BEG
C---- Get index of block address
      call RAYMOND  (WAVE, ITYPE, INDEX)
      OK = INDEX.gt.0
C
      if(OK) then
C----   Read block, and verify WAVE
        call LETTER (KONADR(INDEX), BLOCK, WAVE, 0, 'LAMAR')
C
      else
C----   Failed
        write (MSSLIN(1),100) WAVE,ITYPE
  100   format('WAVE =',1PE24.16,' and ITYPE =',I12,'; unable to find ',
     $         'corresponding Continuum Data block.')
        call HALT   ('LAMAR', 1)
      end if
C     !END
      call BYE ('LAMAR')
C
      return
      end
