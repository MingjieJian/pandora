      subroutine MAPLE
     $(N,GTN,C,EXT,ANT,PA,PG,PB,OMD,DLC,PQ,PD,INC,AW,GAW,PBLOC,
     $ DMP1,DMP2)
C
C     Rudolf Loeser, 1977 Jun 30
C     (This is version 2 of MAPLE.)
C
C---- Frequency(/angle) summation for Line Source Function calculations
C     for a stationary atmosphere.
C     (See also GELON.)
C
C---- INPUT -
C     N   : number of depth points;
C     GTN : (length N), line opacity factor;
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
C     AW  : (length N),   integrated diagonal of WN matrices.
C
C---- DUMP -
C     If DMP1 is true, then completed integrals will be dumped;
C     if DMP2 is true, then detailed intermediates will be dumped.
C
C---- SCRATCH STORAGE -
C     EXT : (length N);
C     C   : (length N), angle integration weights
C           (all =1 here in the stationary case).
C
C---- PBLOC -
C     Input data from PBLOC : PNAME, PHI, PHIW, BC, XKPC, TNU, WN, ALL,
C     XJNU, SIG, T1, T2, and T3. (Note: XJNU, SIG, T1, T2, T3 =0 in the
C     CRD case.)
C
C     [Note: PHI (of size N) and PHIW (of size N*N) must here be
C     distinguished in order to use subroutine "PALLAS".
C     PHI and PHIW differ for expanding atmospheres, but not for
C     stationary atmospheres. Here, each row of PHIW is equal to PHI.]
C
C     PBLOCs are updated by "QUEST."
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, C, DLC, EXT, GTN, OMD, PA, PB, PBLOC, PD, PG, PQ
      integer I, IPEX, LLXI, LPALL, LPBC, LPJNU, LPKPC, LPNAM, LPPHI,
     $        LPPHW, LPSIG, LPT1, LPT2, LPT3, LPTNU, LPWN, N, jummy
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
      equivalence (NNKODS( 1),LLXI  )
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
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
      equivalence (LPD( 1),LPNAM)
      equivalence (LPD(10),LPALL)
      equivalence (LPD( 2),LPPHI)
      equivalence (LPD( 9),LPPHW)
      equivalence (LPD( 8),LPKPC)
      equivalence (LPD( 6),LPSIG)
      equivalence (LPD( 3),LPBC )
      equivalence (LPD(13),LPTNU)
      equivalence (LPD( 4),LPJNU)
      equivalence (LPD( 7),LPWN )
      equivalence (LPD( 5),LPT1 )
      equivalence (LPD(14),LPT2 )
      equivalence (LPD(15),LPT3 )
C     !DASH
      external ONE1, BARNEY, LEYTE, POD, NAGA, RAGE, PALLAS, RAM,
     $         HI, BYE
C
C               PA(N,N), PG(N,N), PB(N), ANT(N), DLC(N), PQ(N), GTN(N),
      dimension PA(*),   PG(*),   PB(*), ANT(*), DLC(*), PQ(*), GTN(*),
C
C               OMD(N), PD(N), C(N), PBLOC(Lpdlen), AW(N), EXT(N)
     $          OMD(*), PD(*), C(*), PBLOC(*),      AW(*), EXT(*)
C     !EJECT
C
      call HI ('MAPLE')
C     !BEG
C---- Initialize dump
      DMPN = DMP1.or.DMP2
      DMPX = (IPEX.lt.0).or.(IPEX.eq.16)
      call NAGA       (DMPN, DMPX, 0, jummy, 1)
C---- Initialize cumulative sums (integrals)
      call BARNEY     (N, INC, GAW, PA, PG, PB, OMD, DLC, PQ, PD, AW,
     $                 ANT)
C---- Set up formal angle-integration weights
      call ONE1       (C, N)
C
      do 100 I = 1,NBOP
        if(KEROP(I).eq.0) then
C----     Process only data from OK blocks
C
C----     Read block
          call LEYTE  (PBLOC, LPDLEN, INDOP(I))
C----     Decode block name
          call POD    (2, PBLOC(LPNAM))
C----     Set up dump heading for this block
          call NAGA   (DMPN, DMPX, I, LLXI, 2)
          if(INC) then
C----       Incident radiation extinction term
            call RAGE (PBLOC(LPTNU), N, EXT)
          end if
C----     Integrate (i.e. add current data to cumulative sums)
          call PALLAS (N, N, INC, GAW, DMP1, DMP2, I, PBLOC(LPALL),
     $                 PBLOC(LPPHI), PBLOC(LPPHW), C, GTN,
     $                 PBLOC(LPKPC), PBLOC(LPBC), PBLOC(LPJNU),
     $                 PBLOC(LPSIG), PBLOC(LPT1), PBLOC(LPT2),
     $                 PBLOC(LPT3), PBLOC(LPWN), PBLOC(LPTNU), EXT,
     $                 ANT, PA, PG, PB, OMD, DLC, PQ, PD, AW)
        end if
  100 continue
C
C---- Flush last of dump (if needed)
      call NAGA       (DMPN, DMPX, 0, jummy, 3)
C
C---- Normalize results
      call RAM        (DMP1, GAW, INC, N, ANT, PA, PG, PB, OMD, DLC,
     $                 PQ, AW, PD)
C     !END
      call BYE ('MAPLE')
C
      return
      end
