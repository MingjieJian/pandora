      subroutine ELKIN
     $(WN,WH,ILFLX,PBLOC,XLTIT)
C
C     Rudolf Loeser, 1983 Feb 04
C---- Saves weight matrices from PRD Continuum Jnu calculation,
C     for use in Line Source function calculation.
C     !DASH
      save
C     !DASH
      real*8 PBLOC, PNAME, WH, WN, XLTIT, ZERO
      integer ILFLX, IOVER, IRAY, KAK1, KIND, LLXI, LPWH, LPWN, MWNSV,
     $        N, N2, NRAY
      character BLANK*1
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(67),MWNSV)
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
      equivalence (LPD( 7),LPWN )
      equivalence (LPD(12),LPWH )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !EJECT
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
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 1),KAK1 )
C
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C     !DASH
C     !EJECT
      external BET, POD, SAMAR, BOHOL, MOVE1, HI, BYE
C
C               PBLOC(Lpdlen), WN(N,N), WH(N,N)
      dimension PBLOC(*),      WN(*),   WH(*)
C
      call HI ('ELKIN')
C     !BEG
      if(IOVER.gt.0) then
        N2 = N**2
C----   Get frequency index from current Continuum Data Block
        call BET     (2, XLTIT)
        LLXI = KAK1
C----   Make name of target Diana Data Block
        IRAY = 0
        NRAY = N
        GENLAB(1) = 'Frequency index'
        GENLAB(2) = BLANK
        GENLAB(3) = 'N'
        GENLAB(4) = BLANK
        call POD     (1, PNAME)
C
C----   Read this Diana Data Block
        call SAMAR   (PNAME, PBLOC, LPDLEN, OPNAM, ZERO, INDOP, MBOP,
     $                KIND)
C
C----   Move matrices into block
        call MOVE1   (WN, N2, PBLOC(LPWN))
        if(ILFLX.gt.0) then
          call MOVE1 (WH, N2, PBLOC(LPWH))
        end if
        MWNSV = MWNSV+1
C
C----   Save updated Diana Data Block
        call BOHOL   (PBLOC, LPDLEN, INDOP(KIND))
      end if
C     !END
      call BYE ('ELKIN')
C
      return
      end
