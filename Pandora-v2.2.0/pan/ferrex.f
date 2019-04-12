      subroutine FERREX
     $(N,NSHL,NRPMX,TSHL,CSHL,CODSRW,Y,MOVING,WNS,WHS,WSHL,ILFLX,W,IW,
     $ TITLE,DUMP)
C
C     Rudolf Loeser, 1983 Mar 15
C---- Computes the total "Shell weight matrices", for HELIOS.
C     !DASH
      save
C     !DASH
      real*8 CODSRW, CSHL, TSHL, W, WHS, WNS, WSHL, Y
      integer ILFLX, IN, IS, ITAU, IW, IWHR, IWHZ, IWNR, IWNZ, IWRK,
     $        MOX, N, NRPMX, NSHL
      logical DUMP, MOVING
      character LABWH*33, LABWN*33, TITLE*(*)
C     !DASH
      external NOOTKA, FLAVIUS, FAKIR, GINA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               [ NP = (N-2)/NTAN + 1 ]
C
C               CSHL(N,NP), WSHL(N,NP), CODSRW(NSHL), TSHL(NRPMX,NSHL),
      dimension CSHL(*),    WSHL(*),    CODSRW(*),    TSHL(*),
C
C               WHS(N,N), WNS(N,N)
     $          WHS(*),   WNS(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IWNZ  ),(IN( 2),IWNR  ),(IN( 3),IWHZ  ),(IN( 4),IWHR  ),
     $(IN( 5),ITAU  ),(IN( 6),IWRK  )
C
      data LABWN /' **********   Integrated WN/Shell'/
      data LABWH /' **********   Integrated WH/Shell'/
C     !EJECT
C
      call HI ('FERREX')
C     !BEG
C     (Get, and allocate, W allotment)
      call NOOTKA  (IN, IS, MOX, 'FERREX')
C
C---- Compute W'Z-matrices directly
      call FLAVIUS (N, NSHL, NRPMX, TSHL, CODSRW, Y, MOVING, W(IWNZ),
     $              W(IWNR), W(IWHZ), W(IWHR), ILFLX, W(ITAU), W(IWRK),
     $              W, IW, DUMP)
C---- Obtain interpolated WN's, as needed
      call FAKIR   (N, NSHL, W(IWNZ), CODSRW, W, DUMP)
C---- Add them up, (and provide dump printout)
      call GINA    (NSHL, N, W(IWNZ), CSHL, WNS, TITLE, LABWN, DUMP)
      if(ILFLX.gt.0) then
C----   Obtain interpolated WH's, as needed
        call FAKIR (N, NSHL, W(IWHZ), CODSRW, W, DUMP)
C----   Add `em up (and provide dump printout)
        call GINA  (NSHL, N, W(IWHZ), WSHL, WHS, TITLE, LABWH, DUMP)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'FERREX')
C     !END
      call BYE ('FERREX')
C
      return
      end
