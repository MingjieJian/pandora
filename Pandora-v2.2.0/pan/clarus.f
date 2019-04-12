      subroutine CLARUS
     $(N,Z,FRR,DX,W)
C
C     Rudolf Loeser, 2000 Jun 13
C---- Drives CAMPION, to compute distances along disk rays.
C     (This is version 2 of CLARUS.)
C     !DASH
      save
C     !DASH
      real*8 DX, FRR, W, Z
      integer IN, IS, ISS, IYY, MOX, N
C     !DASH
      external NEDA, CAMPION, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), DX(N)
      dimension Z(*), DX(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ISS   ),(IN( 2),IYY   )
C
      call HI ('CLARUS')
C     !BEG
C     (Get, and allocate, W allotment)
      call NEDA    (IN, IS, MOX, 'CLARUS', N)
C
      call CAMPION (N, Z, FRR, W(ISS), W(IYY), DX)
C
C     (Give back W allotment)
      call WGIVE   (W, 'CLARUS')
C     !END
      call BYE ('CLARUS')
C
      return
      end
