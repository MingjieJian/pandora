      subroutine BEVEL
     $(N,K,IU,IL,NVY,DUMP,Z,FRR,COPTRN,BCTRN,DP,DW,XNE,MPROM,DDL,FDDL,
     $ CDL,LDL,VEX,V,VR,TE,DL,LTE,GTN,GTNS,STRN,SSTRN,SF,SQ,TF,TQ,W,IW)
C
C     Rudolf Loeser, 2000 Jun 29
C---- Computes intensities and fluxes using spherical coordinates.
C     Returns: SF - shell flux, non-LTE
C              SQ - shell flux, LTE
C              TF - total flux, non-LTE
C              TQ - total flux, LTE.
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, FDDL, FRR, GTN, GTNS,
     $       R1N, SF, SQ, SSTRN, STRN, TE, TF, TQ, V, VEX, VR, W, XNE,
     $       Z
      integer IAD, IAS, IDN, IDQ, II1, IL, IN, IS, ISN, ISQ, IT1, ITN,
     $        ITQ, IU, IW, IWS, JN, K, LDL, MOX, MPROM, MRR, MUX, N,
     $        NVY
      logical DUMP, LTE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(15),MRR)
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
      equivalence (RZQ( 23),R1N  )
C     !DASH
C     !EJECT
      external LABEE, GURU, BELLONA, LEAVES, BLAZE, ZINGEL, WGIVE,
     $         IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               COPTRN(N,KM), BCTRN(N,KM), SSTRN(N,KM), DL(KM), XNE(N),
      dimension COPTRN(*),    BCTRN(*),    SSTRN(*),    DL(*),  XNE(*),
C
C               STRN(N,KM), DDL(LDLMX), CDL(LDLMX), DP(N,LDLMX), TE(N),
     $          STRN(*),    DDL(*),     CDL(*),     DP(*),       TE(*),
C
C               FDDL(N), FRR(MRR), GTNS(N), GTN(N), DW(N), VR(N), Z(N),
     $          FDDL(*), FRR(*),   GTNS(*), GTN(*), DW(*), VR(*), Z(*),
C
C               V(N), VEX(N), SF(KM), SQ(KM), TF(KM), TQ(KM)
     $          V(*), VEX(*), SF(*),  SQ(*),  TF(*),  TQ(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IAS   ),(IN( 2),IAD   ),(IN( 3),IT1   ),(IN( 4),ISN   ),
     $(IN( 5),ISQ   ),(IN( 6),IDN   ),(IN( 7),IDQ   ),(IN( 8),ITN   ),
     $(IN( 9),ITQ   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),II1   )
C     !EJECT
C
      call HI ('BEVEL')
C     !BEG
C     (Get, and allocate W & IW allotments)
      call LABEE    (IN, IS,  MOX, 'BEVEL')
      call GURU     (JN, IWS, MUX, 'BEVEL')
C
C---- Compute flux integration weights
      call BELLONA  (R1N, N, Z, W(IAS), MRR, FRR, W(IAD), W)
C---- Compute Shell and Disk intensities, non-LTE
      call LEAVES   (N, MRR, K, IU, IL, DUMP, R1N, Z, FRR, COPTRN,
     $               BCTRN, GTN, STRN, DP, DW, XNE, MPROM, DDL,
     $               FDDL, CDL, LDL, VEX, V, VR, TE, DL, W(ISN),
     $               IW(II1), W(IT1), W(IDN), IW(II1), W(IT1), W, IW)
C---- Compute flux components, and total flux, non-LTE
      call BLAZE    (N, Z, R1N, MRR, FRR, K, W(ISN), W(IAS), SF,
     $               W(IT1), W(IDN), W(IAD), W(ITN), W(IT1), TF)
C---- Save checksums
      call ZINGEL   (K, N, MRR, W(ISN), W(IDN), SF, W(ITN), TF, NVY,
     $               'G')
      if(LTE) then
C----   Compute Shell and Disk intensities, LTE
        call LEAVES (N, MRR, K, IU, IL, DUMP, R1N, Z, FRR, COPTRN,
     $               BCTRN, GTNS, SSTRN, DP, DW, XNE, MPROM, DDL,
     $               FDDL, CDL, LDL, VEX, V, VR, TE, DL, W(ISQ),
     $               IW(II1), W(IT1), W(IDQ), IW(II1), W(IT1), W, IW)
C----   Compute flux components, and total flux, LTE
        call BLAZE  (N, Z, R1N, MRR, FRR, K, W(ISQ), W(IAS), SQ,
     $               W(IT1), W(IDQ), W(IAD), W(ITQ), W(IT1), TQ)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'BEVEL')
      call IGIVE    (IW, 'BEVEL')
C     !END
      call BYE ('BEVEL')
C
      return
      end
