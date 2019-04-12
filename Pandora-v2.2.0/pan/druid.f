      subroutine DRUID
     $(W,LYNC,WLYNC,XLYNC,K,WVL,DL,YHZ,NFL,WNRML,FNRML,AB)
C
C     Rudolf Loeser, 2005 Nov 04
C---- Calculates H Ly line normalization factors.
C     !DASH
      save
C     !DASH
      real*8 DL, FNRML, W, WLYNC, WNRML, WVL, XLYNC, YHZ
      integer ICHZ, IN, IONF, IPHZ, IS, IWDL, K, LYNC, MOX, NFL
      character AB*1
C     !DASH
      external WOOD, MOVE1, CONADD, FERE, BAUM, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XLYNC(KLYNF), DL(KM), YHZ(KM), WNRML(NFL), FNRML(NFL),
      dimension XLYNC(*),     DL(*),  YHZ(*),  WNRML(*),   FNRML(*),
C
C               WLYNC(KLYNF)
     $          WLYNC(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IWDL  ),(IN( 2),IPHZ  ),(IN( 3),ICHZ  ),(IN( 4),IONF)
C
      call HI ('DRUID')
C     !BEG
C     (Get, and allocate, W allotment)
      call WOOD   (IN, IS, MOX, 'DRUID')
C
C---- Make profile wavelengths table
      call MOVE1  (DL, K, W(IWDL))
      call CONADD (WVL, W(IWDL), K)
C---- Interpolate data to standard wavelengths table
      call FERE   (W(IWDL), 1, YHZ,   1, K,    WNRML, 1, W(IPHZ), 1,
     $             NFL, 1, 1)
      call FERE   (WLYNC,   1, XLYNC, 1, LYNC, WNRML, 1, W(ICHZ), 1,
     $             NFL, 1, 1)
C---- Save old factors
      call MOVE1  (FNRML, NFL, W(IONF))
C---- Compute and print new factors
      call BAUM   (NFL, WNRML, W(IONF), W(IPHZ), W(ICHZ), FNRML,
     $             (WVL+DL(1)), (WVL+DL(K)), AB)
C
C     (Give back W allotment)
      call WGIVE  (W, 'DRUID')
C     !END
      call BYE ('DRUID')
C
      return
      end
