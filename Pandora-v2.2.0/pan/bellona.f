      subroutine BELLONA
     $(R1N,N,Z,AS,MRR,FRR,AD,W)
C
C     Rudolf Loeser, 1981 Sep 08
C---- Computes integration weights for calculating Flux from Intensity,
C     in spherical coordinates.
C     !DASH
      save
C     !DASH
      real*8 AD, AS, FRR, R1N, W, Z
      integer IDR, IDRP, IN, IR, IS, MOX, MRR, N
C     !DASH
      external MINERVA, ELECTRA, MELILOT, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), AS(N), FRR(MRR), AD(MRR)
      dimension Z(*), AS(*), FRR(*),   AD(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IR    ),(IN( 2),IDRP  ),(IN( 3),IDR   )
C
      call HI ('BELLONA')
C     !BEG
C     (Get, and allocate, W allotment)
      call MINERVA   (IN,IS,MOX,'BELLONA')
C
C---- Compute Shell Ray integration weights
      call ELECTRA   (N,R1N,Z,W(IR),AS)
      if(MRR.gt.0) then
C----   Compute Disk Ray integration weights
        call MELILOT (MRR,FRR,R1N,W(IDRP),W(IDR),AD)
      end if
C
C     (Give back W allotment)
      call WGIVE     (W,'BELLONA')
C     !END
      call BYE ('BELLONA')
C
      return
      end
