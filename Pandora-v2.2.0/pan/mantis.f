      subroutine MANTIS
     $(N,HND,XNE,GDENS,PEL,PGS,PTU,PTO,GMASS,Z)
C
C     Rudolf Loeser, 1987 Sep 11
C---- Saves debug checksums, for ZIPPY.
C     (This is version 2 of MANTIS.)
C     !DASH
      save
C     !DASH
      real*8 GDENS, GMASS, HND, PEL, PGS, PTO, PTU, XNE, Z
      integer IOVER, ITHSL, N
      character TIT*32
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(19),ITHSL)
C     !DASH
      external CHECKER, HI, BYE
C
C               HND(N), XNE(N), GDENS(N), PEL(N), PGS(N), PTU(N), Z(N),
      dimension HND(*), XNE(*), GDENS(*), PEL(*), PGS(*), PTU(*), Z(*),
C
C               PTO(N), GMASS(N)
     $          PTO(*), GMASS(*)
C
      call HI ('MANTIS')
C     !BEG
      write (TIT,100) IOVER,ITHSL
  100 format(', H.S.E., IOVER =',I3,', ITHSL =',I3)
C
      call CHECKER (HND  ,1,N,  'HND'//TIT)
      call CHECKER (XNE  ,1,N,  'XNE'//TIT)
      call CHECKER (GDENS,1,N,'GDENS'//TIT)
      call CHECKER (PEL  ,1,N,  'PEL'//TIT)
      call CHECKER (PGS  ,1,N,  'PGS'//TIT)
      call CHECKER (PTU  ,1,N,  'PTU'//TIT)
      call CHECKER (PTO  ,1,N,  'PTO'//TIT)
      call CHECKER (GMASS,1,N,'GMASS'//TIT)
      call CHECKER (Z    ,1,N,    'Z'//TIT)
C     !END
      call BYE ('MANTIS')
C
      return
      end
