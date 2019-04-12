      subroutine GUNDARI
     $(N,I,J,WVL,DL,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,VEX,TE,V,VR,DX,
     $ R1N,ZN,Z,COPTRN,GTN,BCTRN,SUL,FX,GX,DUMP,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Sets up FX (opacity) and GX (monochromatic source function)
C     along a ray for SHELL intensity calculation
C     using spherical geometry - expanding case.
C     See also: FUNADO, KILYDD, ARVON.
C
C     "I" is the index of the depth to which the ray is tangent.
C     "BARBARA" makes tables at RAY points from tables at DEPTH points;
C     such extended tables are distinguished from their depth
C     counterparts by suffix "X". (BARABAS uses BARBARA for the
C     multiple tables of DP.)
C     (This is version 2 of GUNDARI.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, DX, FDDL, FX, GTN, GX,
     $       R1N, SUL, TE, V, VEX, VR, W, WVL, XNE, Z, ZN
      integer I, IBCX, ICOPX, IDPX, IDWX, IEMUX, IFDLX, IGTNX, IN, IS,
     $        ITEX, IVEXX, IVPX, IVRX, IVX, IW, IXNEX, J, LDL, MOX,
     $        MPROM, N, NRP
      logical DUMP, VSW
      character LABEL*(*)
C     !DASH
      external BARBARA, DERBY, BARABAS, ISMENE, KIRGIZ, NUGRIDA, WGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               COPTRN(N,KM), BCTRN(N,KM), DP(N,LDLMX), GTN(N), VEX(N),
      dimension COPTRN(N,*),  BCTRN(N,*),  DP(*),       GTN(*), VEX(*),
C
C               DW(N), CDL(LDLMX), DDL(LDLMX), FX(NRPMX), TE(N), VR(N),
     $          DW(*), CDL(*),     DDL(*),     FX(*),     TE(*), VR(*),
C
C               SUL(NRPMX), DX(NRPMX), GX(NRPMX), Z(N), DL(KM), XNE(N),
     $          SUL(*),     DX(*),     GX(*),     Z(*), DL(*),  XNE(*),
C
C               FDDL(N), V(N)
     $          FDDL(*), V(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),IDPX  ),(IN( 2),IDWX  ),(IN( 3),IVEXX ),(IN( 4),ITEX  ),
     $(IN( 5),IVX   ),(IN( 6),IVRX  ),(IN( 7),IXNEX ),(IN( 8),IFDLX ),
     $(IN( 9),ICOPX ),(IN(10),IBCX  ),(IN(11),IGTNX ),(IN(12),IVPX  ),
     $(IN(13),IEMUX )
C     !EJECT
C
      call HI ('GUNDARI')
C     !BEG
C---- NRP is the number of points along the ray
      NRP  = 2*I+5
C
C     (Get, and allocate, W allotment)
      call NUGRIDA (IN, IS, MOX, 'GUNDARI', NRP)
C
C---- Set up tables at ray points
      call BARABAS (I, DP, N, W(IDPX), NRP, LDL)
C
      call BARBARA (DW,          I, W(IDWX) )
      call BARBARA (VEX,         I, W(IVEXX))
      call BARBARA (TE,          I, W(ITEX) )
      call BARBARA (V,           I, W(IVX)  )
      call BARBARA (VR,          I, W(IVRX) )
      call BARBARA (XNE,         I, W(IXNEX))
      call BARBARA (FDDL,        I, W(IFDLX))
      call BARBARA (COPTRN(1,J), I, W(ICOPX))
      call BARBARA (BCTRN(1,J),  I, W(IBCX) )
      call BARBARA (GTN,         I, W(IGTNX))
C
C---- Compute Mu values
      call KIRGIZ  (R1N, ZN, Z, I, W(IEMUX))
C---- Set up VP, velocity projected along ray
      call DERBY   (W(IVEXX), W(IEMUX), 0, NRP, W(IVPX))
C
C---- Compute FX and GX
      call ISMENE  (NRP, WVL, DL(J), W(IDPX), W(IDWX), W(IXNEX), MPROM,
     $              DDL, W(IFDLX), CDL, LDL, W(IVEXX), W(ITEX), W(IVX),
     $              W(IVRX), W(IVPX), DX, W(IEMUX), W(ICOPX), W(IGTNX),
     $              W(IBCX), SUL, FX, GX, DUMP, LABEL, VSW, W, IW)
C
C     (Give back W allotment)
      call WGIVE   (W, 'GUNDARI')
C     !END
      call BYE ('GUNDARI')
C
      return
      end
