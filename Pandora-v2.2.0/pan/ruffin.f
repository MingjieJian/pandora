      subroutine RUFFIN
     $(OBLOC)
C
C     Rudolf Loeser, 1982 Nov 30
C---- Initializes ORION Data Block records, spherical atmosphere.
C     !DASH
      save
C     !DASH
      real*8 OBLOC
      integer I, IRAY, J, KM, LLXI, MRR, N, NRAY, NSHL
      character BLANK*1
C     !COM
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
C     .
      equivalence
     $(NNKODS( 1),LLXI  ),(NNKODS( 2),IRAY  ),(NNKODS( 3),NRAY  )
C     .
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(15),MRR)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 4),NSHL )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C     !DASH
C     !EJECT
      external TATAR, POD, WEIRDO, CEBU, HI, BYE
C
C               OBLOC(LODLEN)
      dimension OBLOC(*)
C
      call HI ('RUFFIN')
C     !BEG
      GENLAB(1) = 'Frequency index'
      GENLAB(4) = BLANK
C
      NBOP = 0
      LLXI = 0
      do 102 I = 1,KM
        LLXI = LLXI+1
        IRAY = 0
C
        NRAY = 0
        GENLAB(2) = 'Ray index (this is a shell ray)'
        GENLAB(3) = '# of depth points intersected by this ray'
C
        do 100 J = 1,NSHL
          call TATAR  (NRAY)
          call WEIRDO (NBOP, 'RUFFIN', 0)
          IRAY = IRAY+1
          call POD    (1, OPNAM(NBOP))
          OBLOC(1) = OPNAM(NBOP)
          call CEBU   (OBLOC, LODLEN, INDOP(NBOP))
  100   continue
C
        NRAY = N
        GENLAB(2) ='Ray index (this is a disk ray)'
        GENLAB(3) ='N'
C
        do 101 J = 1,MRR
          call WEIRDO (NBOP, 'RUFFIN', 0)
          IRAY = IRAY+1
          call POD    (1, OPNAM(NBOP))
          OBLOC(1) = OPNAM(NBOP)
          call CEBU   (OBLOC, LODLEN, INDOP(NBOP))
  101   continue
C
  102 continue
      call WEIRDO     (NBOP, 'RUFFIN', 1)
      MBOP = NBOP
C     !END
      call BYE ('RUFFIN')
C
      return
      end
