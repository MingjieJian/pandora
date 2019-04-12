      subroutine NAMMA
     $(VE,VP,VH,V1,V2,V3,JQ,JX,VMIN,VMAX)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Gets plot limits for FANDAR.
C     !DASH
      save
C     !DASH
      real*8 V1, V2, V3, VE, VH, VMAX, VMIN, VP
      integer IMAX1, IMAX2, IMAX3, IMAXE, IMAXH, IMAXP, IMIN1, IMIN2,
     $        IMIN3, IMINE, IMINH, IMINP, JQ, JX
C     !DASH
      external  GUARD, HI, BYE
      intrinsic min, max
C
C               VE(N), VP(N), VH(N), V1(N), V2(N), V3(N)
      dimension VE(*), VP(*), VH(*), V1(*), V2(*), V3(*)
C
      call HI ('NAMMA')
C     !BEG
      call GUARD (JQ,JX,VE,IMINE,IMAXE)
      call GUARD (JQ,JX,VP,IMINP,IMAXP)
      call GUARD (JQ,JX,VH,IMINH,IMAXH)
      call GUARD (JQ,JX,V1,IMIN1,IMAX1)
      call GUARD (JQ,JX,V2,IMIN2,IMAX2)
      call GUARD (JQ,JX,V3,IMIN3,IMAX3)
C
      VMIN = min(VE(IMINE),VP(IMINP),VH(IMINH),V1(IMIN1),V2(IMIN2),
     $           V3(IMIN3))
C
      VMAX = max(VE(IMAXE),VP(IMAXP),VH(IMAXH),V1(IMAX1),V2(IMAX2),
     $           V3(IMAX3))
C     !END
      call BYE ('NAMMA')
C
      return
      end
