      subroutine GELON
     $(N,GTN,EXT,ANT,PA,PG,PB,OMD,DLC,PQ,PD,INC,AW,GAW,OBLOC,
     $ DMP1,DMP2)
C
C     Rudolf Loeser, 1981 Dec 04.
C
C---- Frequency/angle summation for Line Source Function calculations
C     for an expanding atmosphere.
C     (See also MAPLE.)
C
C---- INPUT -
C     N   : number of depth points;
C     GTN : (length N), line opacity factor,
C     INC : = true if incident radiation term must be returned;
C     GAW : = true if AW must be returned.
C
C---- OUTPUT -
C     ANT : (length N),   normalization term;
C     PA  : (length N*N), integrated quantity;
C     PG  : (length N*N), integrated quantity;
C     PB  : (length N),   integrated quantity;
C     OMD : (length N),   integrated quantity;
C     DLC : (length N),   integrated quantity;
C     PQ  : (length N),   integrated quantity;
C     PD  : (length N),   integrated quantity;
C     AW  : integrated diagonal of WN matrices.
C
C---- DUMP -
C     If DMP1 is true, then completed integrals will be dumped;
C     if DMP2 is true, then detailed intermediates will be dumped.
C
C---- SCRATCH STORAGE -
C     EXT : (length N).
C
C---- OBLOC -
C     Input data from OBLOC: ONAME, PHI, PHIW, BC, XKPC, TNU, WN, ALL,
C     C, XJNU, SIG, T1, T2, and T3. (XJNU, SIG, T1, T2, T3 all =0 in
C     the CRD case.)
C     Note:  ONAME contains -
C     - LLXI and IRAY, (indices which describe rays in detail); and
C     - NRAY (# of grid points on a ray, .le. N for Shell rays).
C
C     [Note: PHI (of size NRAY) is the symmetric (or "V=0") profile
C     function, while PHIW (of size NRAY*NRAY) is the co-moving
C     profile function.]
C
C     OBLOCs are updated by "BASUTO".
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, DLC, EXT, GTN, OBLOC, OMD, PA, PB, PD, PG, PQ,
     $       dummy
      integer I, IPEX, IRAY, LOALL, LOBC, LOCWT, LOJNU, LOKPC, LONAM,
     $        LOPHI, LOPHIW, LOSIG, LOT1, LOT2, LOT3, LOTNU, LOWN, N,
     $        NRAY
      logical DMP1, DMP2, DMPN, DMPX, GAW, INC
C     !COM
C     !EJECT
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
      equivalence (NNKODS( 3),NRAY  )
      equivalence (NNKODS( 2),IRAY  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C     !EJECT
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
      equivalence (LOD( 1),LONAM )
      equivalence (LOD(12),LOALL )
      equivalence (LOD( 4),LOPHI )
      equivalence (LOD( 2),LOPHIW)
      equivalence (LOD( 5),LOCWT )
      equivalence (LOD( 3),LOKPC )
      equivalence (LOD(11),LOSIG )
      equivalence (LOD( 7),LOBC  )
      equivalence (LOD( 6),LOWN  )
      equivalence (LOD( 9),LOJNU )
      equivalence (LOD( 8),LOTNU )
      equivalence (LOD(10),LOT1  )
      equivalence (LOD(16),LOT2  )
      equivalence (LOD(17),LOT3  )
C     !DASH
      external BARNEY, LEYTE, POD, MUKA, RAGE, PALLAS, RAM, HI, BYE
C
C               PA(N,N), PB(N), ANT(N), DLC(N), OMD(N), EXT(N), GTN(N),
      dimension PA(*),   PB(*), ANT(*), DLC(*), OMD(*), EXT(*), GTN(*),
C
C               OBLOC(Lodlen), PD(N), PQ(N), PG(N,N), AW(N)
     $          OBLOC(*),      PD(*), PQ(*), PG(*),   AW(*)
C     !EJECT
C
      call HI ('GELON')
C     !BEG
C---- Initialize dump
      DMPN = DMP1.or.DMP2
      DMPX = (IPEX.lt.0).or.(IPEX.eq.13)
      call MUKA       (DMPN, DMPX, 0, dummy, 1)
C---- Initialize cumulative sums (integrals)
      call BARNEY     (N, INC, GAW, PA, PG, PB, OMD, DLC, PQ, PD, AW,
     $                 ANT)
C
      do 100 I = 1,NBOP
        if(KEROP(I).eq.0) then
C----     Process only data from OK blocks
C
C----     Read block
          call LEYTE  (OBLOC, LODLEN, INDOP(I))
C----     Decode block name
          call POD    (2, OBLOC(LONAM))
C----     Set up dump heading for this block
          call MUKA   (DMPN, DMPX, I, OBLOC(LONAM), 2)
          if(INC.and.(IRAY.eq.1)) then
C----       Incident radiation extinction term
            call RAGE (OBLOC(LOTNU), N, EXT)
          end if
C----     Integrate (i.e. add current data to cumulative sums)
          call PALLAS (NRAY, N, INC, GAW, DMP1, DMP2, I, OBLOC(LOALL),
     $                 OBLOC(LOPHI), OBLOC(LOPHIW), OBLOC(LOCWT),
     $                 GTN, OBLOC(LOKPC), OBLOC(LOBC), OBLOC(LOJNU),
     $                 OBLOC(LOSIG), OBLOC(LOT1), OBLOC(LOT2),
     $                 OBLOC(LOT3), OBLOC(LOWN), OBLOC(LOTNU), EXT,
     $                 ANT, PA, PG, PB, OMD, DLC, PQ, PD, AW)
        end if
  100 continue
C
C---- Flush last of dump (if needed)
      call MUKA       (DMPN, DMPX, 0, dummy, 3)
C
C---- Normalize results
      call RAM        (DMP1, GAW, INC, N, ANT, PA, PG, PB, OMD, DLC,
     $                 PQ, AW, PD)
C     !END
      call BYE ('GELON')
C
      return
      end
