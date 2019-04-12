      subroutine MELATA
     $(N,CVX,VX,HND,NFH,HNDF,FNH,VEC)
C
C     Rudolf Loeser, 2005 Jan 05
C---- Provides for minimum chromospheric and transition region flow
C     velocity in "additional expansion velocity."
C     !DASH
      save
C     !DASH
      real*8 CVX, FL, FNH, HND, HNDF, VEC, VH, VX, ZERO
      integer I, N, NFH, jummy
      logical PCVX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  LOGO, LININT, HI, BYE
      intrinsic min, max
C
C               VX(N), HND(N), HNDF(NFH), FNH(NFH), VEC(NFH)
      dimension VX(*), HND(*), HNDF(*),   FNH(*),   VEC(*)
C
      call HI ('MELATA')
C     !BEG
      if(NFH.gt.0) then
        call LOGO     (HNDF, NFH, 0, ZERO, VEC)
        PCVX = CVX.ge.ZERO
C
        do 100 I = 1,N
          VH = log10(HND(I))
          call LININT (VEC, 1, FNH, 1, NFH, VH, FL, 1, 1, jummy)
          if(PCVX) then
            VX(I) = max(VX(I),(+FL))
          else
            VX(I) = min(VX(I),(-FL))
          end if
  100   continue
      end if
C     !END
      call BYE ('MELATA')
C
      return
      end
