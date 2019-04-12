      subroutine BELI
     $(N,DX,DW,DP,XNE,MPROM,DDL,FDDL,CDL,LDL,TE,V,VR,VP,EMU,WVL,DLJ,
     $ PHI,DWU,LABEL,VSW,PRNT,W,IW)
C
C     Rudolf Loeser, 2000 Jun 15
C---- Computes PHI, Line Absorption Profile (and recomputes
C     Doppler Width as needed)
C     for intensity calculations using spherical geometry.
C     (This is version 2 of BELI.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DLJ, DP, DW, DWU, DX, EMU, FDDL, PHI, TE, V, VP,
     $       VR, W, WVL, XNE
      integer IDWIN, IW, LDL, MPROM, N
      logical DUMP, PRNT, VSW
      character LABEL*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 75),IDWIN)
C     !DASH
      external TALUS, MOVE1, YMUIR, KELLI, HI, BYE
C
      dimension W(*), IW(*)
C
C               DP(*,LDLMX), DDL(LDLMX), CDL(LDLMX)
      dimension DP(N,*),     DDL(*),     CDL(*)
C
      dimension DWU(*), TE(*), VR(*), PHI(*), VP(*), V(*), DX(*), DW(*),
     $          XNE(*), EMU(*), FDDL(*)
C     !EJECT
C
      call HI ('BELI')
C     !BEG
C---- Set up Doppler Width
      if(VSW) then
        call TALUS (TE, V, VR, EMU, N, WVL, 1, DWU)
      else
        call MOVE1 (DW, N, DWU)
      end if
C---- Compute Absorption Profile
      call YMUIR   (W, IW, WVL, DLJ, 1, DP, DWU, XNE, VP, N, DDL, FDDL,
     $              CDL, LDL, MPROM, PHI)
C
      DUMP = PRNT.and.VSW.and.(IDWIN.gt.0)
      if(DUMP) then
        call KELLI (N, DX, TE, V, VR, VP, EMU, DWU, DP, PHI, LABEL)
      end if
C     !END
      call BYE ('BELI')
C
      return
      end
