      subroutine SPLEEN
     $(WAVES,YHZ,YAN,BT,IS,NB,YHZA,YANA,BTA)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Computes averages, for HEART.
C     !DASH
      save
C     !DASH
      real*8 BT, BTA, WAVES, YAN, YANA, YHZ, YHZA
      integer IS, NB
C     !DASH
      external CHELA, HI, BYE
C
C               WAVES(NCP), YHZ(NCP), YAN(NCP), BT(NCP)
      dimension WAVES(*),   YHZ(*),   YAN(*),   BT(*)
C
      call HI ('SPLEEN')
C     !BEG
      call CHELA (WAVES(IS),YHZ(IS),NB,YHZA)
      call CHELA (WAVES(IS),YAN(IS),NB,YANA)
      call CHELA (WAVES(IS),BT(IS) ,NB,BTA )
C     !END
      call BYE ('SPLEEN')
C
      return
      end
