      subroutine BAKA
     $(LINES)
C
C     Rudolf Loeser, 1998 Jan 13
C---- Encodes scratch file usage information.
C     (This is version 2 of BAKA.)
C     !DASH
      save
C     !DASH
      real*8 BLKS, DISK, ONE, RAML, RAMU, ZERO
      character LINES*38
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- CORTEX      as of 1997 Jun 16
      integer     MELEFT,MENEXT,MENRAC,MENRUP,MENRRE,MELIMT
      real*8      SIOBUF,SENWAC,SENWUP,SENWRE
C
      dimension   SIOBUF( 1 )
C     (The   r e a l   length of SIOBUF is set in PANDORA!)
C
      common      /CORTEX1/ MELEFT,MENEXT,MENRAC,MENRUP,MENRRE
      common      /CORTEX2/ SENWAC,SENWUP,SENWRE
      common      /CORTEX3/ MELIMT
      common      /CORTEX4/ SIOBUF
C
C     Control parameters for the MEMOIR subroutines:
C
C     MELIMT = length of entire buffer (SIOBUF), in words;
C     MELEFT = number of unused words left in buffer;
C     MENEXT = index of next available word in buffer;
C     MENRAC = number of logical records accepted;
C     MENRUP = number of logical records updated;
C     MENRRE = number of logical records returned;
C     SENWAC = number of words accepted;
C     SENWUP = number of words updated;
C     SENWRE = number of words returned.
C     .
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C     !DASH
      external  HI, BYE
      intrinsic max
C
      dimension LINES(3)
C     !EJECT
C
      call HI ('BAKA')
C     !BEG
      BLKS = max((FILDAT(2)-ONE),ZERO)
      DISK = BLKS*FILDAT(3)
      write (LINES(1),101) DISK
  101 format(' Disk scratch file use:',1PE9.2,' bytes')
C
      RAMU = 8*(MENEXT-1)
      write (LINES(2),102) RAMU
  102 format('  RAM scratch file use:',1PE9.2,' bytes')
C
      RAML = 8*MELIMT
      write (LINES(3),103) RAML
  103 format('      RAM buffer limit:',1PE9.2,' bytes')
C     !END
      call BYE ('BAKA')
C
      return
      end
