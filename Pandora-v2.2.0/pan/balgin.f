      subroutine BALGIN
     $(WN,WH,ILFLX,PHI,MM,I,OBLOC,XLTIT)
C
C     Rudolf Loeser, 1983 Feb 04
C---- Saves weight matrices and a co-moving-phi-matrix from continuum
C     JNU calculation, for use in line source function calculation.
C     !DASH
      save
C     !DASH
      real*8 OBLOC, ONAME, PHI, WH, WN, XLTIT, ZERO
      integer I, I2, ILFLX, IRAY, KAK1, KIND, LLXI, LOPHIW, LOWH, LOWN,
     $        MM, MWNSV, NRAY
C     !COM
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
      equivalence (LOD( 6),LOWN  )
      equivalence (LOD(14),LOWH  )
      equivalence (LOD( 2),LOPHIW)
C
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
C     !EJECT
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 1),KAK1 )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (LEST(67),MWNSV)
C     !DASH
C     !EJECT
      external BET, POD, SAMAR, BOHOL, MOVE1, HI, BYE
C
C               OBLOC(Lodlen), WN(I,I), WH(I,I), PHI(I,I)
      dimension OBLOC(*),      WN(*),   WH(*),   PHI(*)
C
      call HI ('BALGIN')
C     !BEG
      I2 = I**2
C---- Get frequency index, from current Continuum Data block
      call BET     (2, XLTIT)
C---- Make name of target Orion Data block
      LLXI = KAK1
      IRAY = MM
      NRAY = I
      call POD     (1, ONAME)
C
C---- Read this Orion Data block
      call SAMAR   (ONAME, OBLOC, LODLEN, OPNAM, ZERO, INDOP, MBOP,
     $              KIND)
C
C---- Move data into block
      call MOVE1   (WN , I2, OBLOC(LOWN  ))
      call MOVE1   (PHI, I2, OBLOC(LOPHIW))
      if(ILFLX.gt.0) then
        call MOVE1 (WH , I2, OBLOC(LOWH  ))
      end if
      MWNSV = MWNSV+1
C
C---- Save updated Orion Data block
      call BOHOL   (OBLOC, LODLEN, INDOP(KIND))
C     !END
      call BYE ('BALGIN')
C
      return
      end
