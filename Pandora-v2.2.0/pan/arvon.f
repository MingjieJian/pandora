      subroutine ARVON
     $(N,J,DL,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,VEX,TE,V,VR,DX,R1N,FRR,
     $ Z,WVL,COPTRN,GTN,BCTRN,SUL,FX,GX,DUMP,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 16
C---- Sets up FX (opacity) and GX (monochromatic source function)
C     along a ray for DISK intensity calculation
C     using spherical geometry - expanding case.
C     See also: GUNDARI, FUNADO, and KILYDD.
C     (This is version 2 of ARVON.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, DX, FDDL, FRR, FX,
     $       GTN, GX, R1N, SUL, TE, V, VEX, VR, W, WVL, XNE, Z
      integer IEMU, IN, IS, IVP, IW, J, LDL, MOX, MPROM, N, NRP
      logical DUMP, VSW
      character LABEL*(*)
C     !DASH
      external KALMYK, DERBY, ISMENE, KYLVON, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               COPTRN(N,KM), BCTRN(N,KM), DP(N,LDLMX), FDDL(N), DW(N),
      dimension COPTRN(N,*),  BCTRN(N,*),  DP(*),       FDDL(*), DW(*),
C
C               DDL(LDLMX), DL(KM), VEX(N), XNE(N), SUL(NRPMX), GTN(N),
     $          DDL(*),     DL(*),  VEX(*), XNE(*), SUL(*),     GTN(*),
C
C               DX(NRPMX), FX(NRPMX), CDL(LDLMX), TE(N), VR(N), V(N),
     $          DX(*),     FX(*),     CDL(*),     TE(*), VR(*), V(*),
C
C               GX(NRPMX), Z(N)
     $          GX(*),     Z(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IVP   ),(IN( 2),IEMU  )
C     !EJECT
C
      call HI ('ARVON')
C     !BEG
C---- NRP is the number of points along the ray
      NRP = N
C
C     (Get, and allocate, W allotment)
      call KYLVON (IN, IS, MOX, 'ARVON', NRP)
C
C---- Compute Mu values
      call KALMYK (R1N, FRR, Z, N, W(IEMU))
C---- Set up VP (velocity projected along ray)
      call DERBY  (VEX, W(IEMU), 0, N, W(IVP))
C
C---- Compute FX and GX
      call ISMENE (NRP, WVL, DL(J), DP, DW, XNE, MPROM, DDL, FDDL, CDL,
     $             LDL, VEX, TE, V, VR, W(IVP), DX, W(IEMU),
     $             COPTRN(1,J), GTN, BCTRN(1,J), SUL, FX, GX, DUMP,
     $             LABEL, VSW, W, IW)
C
C     (Give back W allotment)
      call WGIVE  (W, 'ARVON')
C     !END
      call BYE ('ARVON')
C
      return
      end
