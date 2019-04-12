      subroutine BOLL
     $(N,Z,ZL,XNE,XNP,ZHEL,ZHEV,LU,W)
C
C     Rudolf Loeser, 1984 May 11
C---- Plots electrons, and contributors, for POCKET.
C     !DASH
      save
C     !DASH
      real*8 W, XNE, XNP, Z, ZHEL, ZHEV, ZL
      integer IHLL, IHVL, IN, INEL, INPL, IS, LU, MOX, N
C     !DASH
      external WEEVIL, SNOUT, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), ZL(N), XNE(N), XNP(N), ZHEL(N), ZHEV(N)
      dimension Z(*), ZL(*), XNE(*), XNP(*), ZHEL(*), ZHEV(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),INEL  ),(IN( 2),INPL  ),(IN( 3),IHLL  ),(IN( 4),IHVL  )
C
      call HI ('BOLL')
C     !BEG
C     (Get, and allocate, W allotment)
      call WEEVIL (IN,IS,MOX,'BOLL')
C
      call SNOUT  (N,Z,ZL,XNE,W(INEL),XNP,W(INPL),ZHEL,W(IHLL),ZHEV,
     $             W(IHVL),LU)
C
C     (Give back W allotment)
      call WGIVE  (W,'BOLL')
C     !END
      call BYE ('BOLL')
C
      return
      end
