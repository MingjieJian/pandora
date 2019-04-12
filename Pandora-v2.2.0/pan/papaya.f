      subroutine PAPAYA
     $(J,XAX,RCP,YR,MR,MRP,MRJ,XAXIJ,RRCPIJ,YRATE)
C
C     Rudolf Loeser, 18 May 71
C---- Gets rates integrations parameters.
C
C---- XAXIJ is either RRNUIJ or WRATIJ.
C     !DASH
      save
C     !DASH
      real*8 RCP, RRCPIJ, XAX, XAXIJ, YR, YRATE
      integer J, L, MR, MRJ, MRP, jummy
C     !DASH
      external MINK, MOVE1, HI, BYE
C
C               MRJ(NSL+1), RRCPIJ(MRS), RCP(MRX), YRATE(MRS), YR(MRX),
      dimension MRJ(*),     RRCPIJ(*),   RCP(*),   YRATE(*),   YR(*),
C
C               XAX(MRX), XAXIJ(MRS)
     $          XAX(*),   XAXIJ(*)
C
      call HI ('PAPAYA')
C     !BEG
      call MINK    (J, MRJ, jummy, L)
      XAX(1) = XAXIJ (L)
      RCP(1) = RRCPIJ(L)
      YR (1) = YRATE (L)
C
      MR = MRJ(J)
C
      if(MR.gt.0) then
        call MOVE1 (XAXIJ (L+1), MR, XAX(2))
        call MOVE1 (RRCPIJ(L+1), MR, RCP(2))
        call MOVE1 (YRATE (L+1), MR, YR (2))
      end if
C
      MRP = 1+MR
C     !END
      call BYE ('PAPAYA')
C
      return
      end
