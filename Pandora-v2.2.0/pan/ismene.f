      subroutine ISMENE
     $(NRP,WVL,DLJ,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,VEX,TE,V,VR,VP,DX,
     $ EMU,COP,GTN,BC,SUL,FX,GX,DUMP,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 13
C---- Sets up FX (opacity) and GX (monochromatic source function) for
C     intensity calculation along a ray in spherical geometry.
C     (This is version 2 of ISMENE.)
C     !DASH
      save
C     !DASH
      real*8 BC, CDL, COP, DDL, DLJ, DP, DW, DX, EMU, FDDL, FX, GTN, GX,
     $       SUL, TE, V, VEX, VP, VR, W, WVL, XNE
      integer IDWU, IGTU, IN, IPHI, IS, IVEC, IW, LDL, MOX, MPROM, NRP
      logical DUMP, VSW
      character LABEL*(*)
C     !DASH
      external BELI, ELSI, SOUFFLE, GOLDI, TURKANA, LOTOR, WGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
      dimension GX(*), BC(*), COP(*), DDL(*), V(*), XNE(*), FDDL(*),
     $          CDL(*), VEX(*), TE(*), VR(*), EMU(*), DW(*), SUL(*),
     $          GTN(*), DX(*), FX(*), VP(*), DP(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),IDWU  ),(IN( 3),IGTU  ),(IN( 4),IVEC  )
C
      call HI ('ISMENE')
C     !BEG
C     (Get, and allocate, W allotment)
      call LOTOR   (IN, IS, MOX, 'ISMENE', NRP)
C
C---- Compute Line Opacity, (and DW as needed)
      call BELI    (NRP, DX, DW, DP, XNE, MPROM, DDL, FDDL, CDL, LDL,
     $              TE, V, VR, VP, EMU, WVL, DLJ, W(IPHI), W(IDWU),
     $              LABEL, VSW, DUMP, W, IW)
C---- Set up GTN-used (possibly modified by DW-ratio)
      call TURKANA (NRP, DW, W(IDWU), GTN, W(IGTU))
C---- Compute Total Opacity (FX)
      call ELSI    (1, NRP, W(IPHI), COP, W(IGTU), W(IVEC), FX)
C---- Compute Monochromatic Source Function (GX)
      call SOUFFLE (1, NRP, COP, W(IGTU), W(IPHI), BC, SUL, FX, GX)
      if(DUMP) then
        call GOLDI (NRP, LABEL, EMU, VEX, VP, W(IGTU), W(IPHI), COP,
     $              BC, SUL, FX, GX)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'ISMENE')
C     !END
      call BYE ('ISMENE')
C
      return
      end
