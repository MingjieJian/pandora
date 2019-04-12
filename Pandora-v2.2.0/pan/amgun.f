      subroutine AMGUN
     $(N,K,XOBL,SNU,H)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Controls calculation of H, for ISHIM.
C     (This is version 2 of AMGUN.)
C     !DASH
      save
C     !DASH
      real*8 H, SNU, XOBL
      integer I, IRAY, J, K, LLXI, LONAM, LOWH, LOWWT, N, NRAY
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
      equivalence (LOD( 1),LONAM )
      equivalence (LOD(15),LOWWT )
      equivalence (LOD(14),LOWH  )
C     !DASH
C     !EJECT
      external ZERO1, LEYTE, POD, CHULYM, HI, BYE
C
C               SNU(N,K), H(N,K), XOBL(Lodlen)
      dimension SNU(N,*), H(N,*), XOBL(*)
C
      call HI ('AMGUN')
C     !BEG
      call ZERO1      (H, (N*K))
      do 100 I = 1,NBOP
        if(KEROP(I).eq.0) then
          call LEYTE  (XOBL, LODLEN, INDOP(I))
          call POD    (2, XOBL(LONAM))
          if(IRAY.eq.1) then
            J = LLXI
          end if
          call CHULYM (NRAY, XOBL(LOWWT), XOBL(LOWH), SNU(1,J), H(1,J))
        end if
  100 continue
C     !END
      call BYE ('AMGUN')
C
      return
      end
