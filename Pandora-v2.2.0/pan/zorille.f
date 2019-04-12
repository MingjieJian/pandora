      subroutine ZORILLE
     $(MTR,KIJ,CRD,CVW,CSK,CRS,DPC,PRD,GMA,DPM,PXC,PXP,PXR)
C
C     Rudolf Loeser, 1992 Mar 18
C---- Eliminates ATOM printout data for THICK transitions.
C     (This is version 2 of ZORILLE.)
C     !DASH
      save
C     !DASH
      real*8 CRD, CRS, CSK, CVW, DPC, DPM, GMA, PRD, PXC, PXP, PXR,
     $       ZERO
      integer I, KIJ, MTR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               MUL = NL*(NL-1)/2
C
C               KIJ(MUL), CRD(MUL), CVW(MUL), CSK(MUL), DPC(MUL),
      dimension KIJ(*),   CRD(*),   CVW(*),   CSK(*),   DPC(*),
C
C               GMA(MUL), PXP(MUL), CRS(MUL), PRD(MUL), DPM(MUL),
     $          GMA(*),   PXP(*),   CRS(*),   PRD(*),   DPM(*),
C
C               PXC(MUL), PXR(MUL)
     $          PXC(*),   PXR(*)
C
      call HI ('ZORILLE')
C     !BEG
      do 100 I = 1,MTR
        if(KIJ(I).eq.5) then
          CRD(I) = ZERO
          CVW(I) = ZERO
          CSK(I) = ZERO
          CRS(I) = ZERO
          DPC(I) = ZERO
          PRD(I) = ZERO
          GMA(I) = ZERO
          DPM(I) = ZERO
          PXC(I) = ZERO
          PXP(I) = ZERO
          PXR(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('ZORILLE')
C
      return
      end
