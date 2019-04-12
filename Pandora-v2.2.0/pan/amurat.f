      subroutine AMURAT
     $(XPBL)
C
C     Rudolf Loeser, 1982 Nov 30
C---- Initializes DIANA Data Block records.
C     !DASH
      save
C     !DASH
      real*8 XPBL
      integer I, IRAY, KM, LLXI, N, NRAY
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
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
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
      external POD, WEIRDO, CEBU, HI, BYE
C
C               XPBL(Lpdlen)
      dimension XPBL(*)
C
C
      call HI ('AMURAT')
C     !BEG
      GENLAB(1) = 'Frequency index'
      GENLAB(2) = BLANK
      GENLAB(3) = 'N'
      GENLAB(4) = BLANK
C
      NBOP = 0
      LLXI = 0
      IRAY = 0
      NRAY = N
C
      do 100 I = 1,KM
        call WEIRDO (NBOP, 'AMURAT', 0)
        LLXI = LLXI+1
        call POD    (1, OPNAM(NBOP))
        XPBL(1) = OPNAM(NBOP)
        call CEBU   (XPBL, LPDLEN, INDOP(NBOP))
  100 continue
C
      call WEIRDO   (NBOP, 'AMURAT', 1)
      MBOP = NBOP
C     !END
      call BYE ('AMURAT')
C
      return
      end
