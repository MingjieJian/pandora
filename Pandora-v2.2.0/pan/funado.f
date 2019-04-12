      subroutine FUNADO
     $(N,I,J,WVL,DL,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,VEX,TE,V,VR,DX,R1N,
     $ ZN,Z,COPTRN,GTN,BCTRN,SUL,FX,GX,DUMP,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Sets up FX (opacity) and GX (monochromatic source function)
C     along a ray for SHELL intensity calculation
C     using spherical geometry - static case.
C     See also: GUNDARI, KILYDD, ARVON.
C
C---- I is the index of the radius to which this ray is tangent.
C
C---- Subsequent processing in CASTOR uses tables set up along
C     extended rays (which have 2*I+5 data points), as in the MOVING
C     case. Thus, as the final step in FUNADO, the computed tables
C     of KPNU and SNU are converted to the form of extended rays,
C     returned in FX and GX.
C     (This is version 2 of FUNADO.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, DX, FDDL, FX, GTN, GX,
     $       R1N, SUL, TE, V, VEX, VR, W, WVL, XNE, Z, ZN
      integer I, IEMU, IKPNU, IN, IS, ISNU, IVP, IW, J, LDL, MOX, MPROM,
     $        N, NRP
      logical DUMP, VSW
      character LABEL*(*)
C     !DASH
      external LEBED, MOVE1, BARBARA, ISMENE, NUFODA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               FX(NRPMX), BCTRN(N,KM), COPTRN(N,KM), XNE(N), CDL(LDL),
      dimension FX(*),     BCTRN(N,*),  COPTRN(N,*),  XNE(*), CDL(*),
C
C               DP(N,LDL), DW(N), DDL(LDL), VEX(N), FDDL(N), GX(NRPMX),
     $          DP(*),     DW(*), DDL(*),   VEX(*), FDDL(*), GX(*),
C
C               VR(N), DL(KM), DX(NRPMX), GTN(N), SUL(NRPMX), TE(N),
     $          VR(*), DL(*),  DX(*),     GTN(*), SUL(*),     TE(*),
C
C               Z(N), V(N)
     $          Z(*), V(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IVP   ),(IN( 2),IEMU  ),(IN( 3),ISNU  ),(IN( 4),IKPNU  )
C     !EJECT
C
      call HI ('FUNADO')
C     !BEG
C---- NRP is the number of points along the ray
      NRP = I
C
C     (Get, and allocate, W allotment)
      call NUFODA  (IN, IS, MOX, 'FUNADO', NRP)
C
C---- Compute Mu values
      call LEBED   (N, Z, W(IEMU), ZN, R1N)
C---- Set up VP, velocity projected along ray
C     (Note: VEX=VP=0, because this is the static case)
      call MOVE1   (VEX, N, W(IVP))
C
C---- Compute KPNU (opacity) and SNU (monochromatic source function)
      call ISMENE  (NRP, WVL, DL(J), DP, DW, XNE, MPROM, DDL, FDDL,
     $              CDL, LDL, VEX, TE, V, VR, W(IVP), DX, W(IEMU),
     $              COPTRN(1,J), GTN, BCTRN(1,J), SUL, W(IKPNU),
     $              W(ISNU), DUMP, LABEL, VSW, W, IW)
C---- Set up results along an extended ray, as in the moving case
      call BARBARA (W(IKPNU), I, FX)
      call BARBARA (W(ISNU),  I, GX)
C
C     (Give back W allotment)
      call WGIVE   (W, 'FUNADO')
C     !END
      call BYE ('FUNADO')
C
      return
      end
