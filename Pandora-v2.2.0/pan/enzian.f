      subroutine ENZIAN
     $(N,LDL,NO,IFBRSW,MPROM,Z,TE,XNE,XN1,HN1,DP,DPM,DW,CRD,CVW,CSK,CRS,
     $ CIB,FF,TRD,TVW,TSK,TRS,TIB,CC)
C
C     Rudolf Loeser, 1988 Jul 19
C---- Computes and prints damping parameter
C     for every component of the current transition.
C     !DASH
      save
C     !DASH
      real*8 CC, CIB, CRD, CRS, CSK, CVW, DP, DPM, DW, FF, HN1, TE, TIB,
     $       TRD, TRS, TSK, TVW, XN1, XNE, Z
      integer IFBRSW, L, LDL, LSAME, MPROM, N, NO
      logical COMPUTE
C     !DASH
      external KELEK, GROUSE, OLIVINE, AMULET, HI, BYE
C
C               XNE(N), TE(N), XN1(N), HN1(N), FF(5), CC(5), DP(N,LDL),
      dimension XNE(*), TE(*), XN1(*), HN1(*), FF(*), CC(*), DP(N,*),
C
C               TVW(N), CRD(LDL), CVW(LDL), CSK(LDL), TRD(N), TIB(N),
     $          TVW(*), CRD(*),   CVW(*),   CSK(*),   TRD(*), TIB(*),
C
C               DW(N), TSK(N), TRS(N), Z(N)
     $          DW(*), TSK(*), TRS(*), Z(*)
C
      call HI ('ENZIAN')
C     !BEG
      do 100 L = 1,LDL
        call KELEK     (L, LDL, N, CRD, CVW, CSK, DP, LSAME, COMPUTE)
        if(COMPUTE) then
C----     Get damping components coefficients
          call GROUSE  (IFBRSW, CRD(L), CVW(L), CSK(L), CRS, CIB, FF)
C----     Compute
          call OLIVINE (TRD, TVW, TSK, TRS, TIB, FF, DPM, CC, DP(1,L))
        end if
C----   Print
        call AMULET    (NO, L, LDL, COMPUTE, LSAME, MPROM, FF, DPM,
     $                  CC, Z, TE, XNE, XN1, HN1, DP(1,L), DW,
     $                  TRD, TVW, TSK, TRS, TIB)
  100 continue
C     !END
      call BYE ('ENZIAN')
C
      return
      end
