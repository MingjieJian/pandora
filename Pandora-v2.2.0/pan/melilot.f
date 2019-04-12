      subroutine MELILOT
     $(MRR,FRR,R1N,DRP,DR,A)
C
C     Rudolf Loeser, 1981 Jul 01
C---- Computes weights, A, (and intermediates DRP and DR),
C     for integration over annuli.
C     !DASH
      save
C     !DASH
      real*8 A, DR, DRI, DRM, DRP, FRR, HALF, R1N, ZERO
      integer I, MO, MRR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               FRR(MRR), DRP(MRR), DR(MRR), A(MRR)
      dimension FRR(*),   DRP(*),   DR(*),   A(*)
C     !EJECT
C
      call HI ('MELILOT')
C     !BEG
      if(MRR.le.1) then
C
        DRP(1) = ZERO
        DR(1)  = ZERO
        A(1)   = R1N**2
C
      else
C
        do 100 I = 1,MRR
          DRP(I) = R1N*FRR(I)
  100   continue
C
        MO = MRR-1
C
        do 101 I = 1,MO
          DR(I) = HALF*(DRP(I)+DRP(I+1))
  101   continue
        DR(MRR) = ZERO
C
        DRM  = DR(1)**2
        A(1) = DRM
        if(MO.ge.2) then
          do 102 I = 2,MO
            DRI  = DR(I)**2
            A(I) = DRI-DRM
            DRM  = DRI
  102     continue
        end if
        A(MRR) = R1N**2-DRM
C
      end if
C     !END
      call BYE ('MELILOT')
C
      return
      end
