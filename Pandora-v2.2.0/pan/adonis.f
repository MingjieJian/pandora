      subroutine ADONIS
C
C     Rudolf Loeser, 1984 Feb 21
C---- Prints the random access file index for Continuum Data Blocks.
C     (This is version 2 of ADONIS.)
C     !DASH
      save
C     !DASH
      integer I, LUEO, NCHR
      character BREAKDOWN*35
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
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external SHIM, ECHO, LINER, HI, BYE
C
      call HI ('ADONIS')
C     !BEG
      write (LUEO,100) NUMKON,KONWAL,WAVEDEL
  100 format(' ','Random Access File Record Index of Continuum Data ',
     $           'Blocks.'//
     $       ' ',9X,'# of entries =',I6,11X,'LIMIT =',I6,
     $           9X,'WAVEDEL =',1PE10.3//
     $       ' ',19X,'Wavelength',20X,'Name',3X,'Address',8X,'Type',
     $           3X,'LIC',2X,'NSH',4X,'(All [shared] types)')
      call LINER  (1, LUEO)
C
      do 102 I = 1,NUMKON
        call ECHO (KONTYP(I), BREAKDOWN, NCHR, 35)
        write (LUEO,101) I,CONWAV(I),CONTIT(I),KONADR(I),KONTYP(I),
     $                   KONLIC(I),KONNSH(I),BREAKDOWN(:NCHR)
  101   format(' ',I5,1PE24.16,0PF24.0,I10,I12,I6,I5,4X,'(',A,')')
        call SHIM (I, 5, LUEO)
  102 continue
C     !END
      call BYE ('ADONIS')
C
      return
      end
