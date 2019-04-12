      subroutine CHUNA
     $(N,K,PBLOC,C,SNU,H,PHI)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Computes H, and gets PHI, for TOBOL.
C     (This is version 2 of CHUNA.)
C     !DASH
      save
C     !DASH
      real*8 C, H, PBLOC, PHI, SNU
      integer I, K, LLXI, LPNAM, LPPHI, LPWH, N, NK
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
      equivalence (NNKODS( 1),LLXI  )
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
      equivalence (LPD( 1),LPNAM)
      equivalence (LPD( 2),LPPHI)
      equivalence (LPD(12),LPWH )
C     !DASH
C     !EJECT
      external ONE1, ZERO1, LEYTE, POD, CHULYM, MOVE1, HI, BYE
C
C               SNU(N,K), H(N,K), C(N), PHI(N,K), PBLOC(Lpdlen)
      dimension SNU(N,*), H(N,*), C(*), PHI(N,*), PBLOC(*)
C
      call HI ('CHUNA')
C     !BEG
      NK = N*K
      call ONE1       (C,   N )
      call ZERO1      (H,   NK)
      call ZERO1      (PHI, NK)
C
      do 100 I = 1,NBOP
        if(KEROP(I).eq.0) then
          call LEYTE  (PBLOC, LPDLEN, INDOP(I))
          call POD    (2, PBLOC(LPNAM))
          call MOVE1  (PBLOC(LPPHI), N, PHI(1,LLXI))
          call CHULYM (N, C, PBLOC(LPWH), SNU(1,LLXI), H(1,LLXI))
        end if
  100 continue
C     !END
      call BYE ('CHUNA')
C
      return
      end
