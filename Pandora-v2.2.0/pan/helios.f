      subroutine HELIOS
     $(N,NSHL,NRPMX,MRR,Y,MOVING,WN,WH,ILFLX,TSHL,CSHL,TDSK,CDSK,
     $ WNS,WND,WHS,WHD,CODSRW,WSHL,WDSK,W,IW,TITLE)
C
C     Rudolf Loeser, 1981 Nov 02
C---- Computes a WN-matrix, and a WH-matrix (if needed).
C     (This is version 4 of HELIOS.)
C     !DASH
      save
C     !DASH
      real*8 CDSK, CODSRW, CSHL, TDSK, TSHL, W, WDSK, WH, WHD, WHS, WN,
     $       WND, WNS, WSHL, Y
      integer ILFLX, IW, MRR, N, N2, NRPMX, NSHL
      logical DUMP, MOVING, TOPT
      character LABWH*23, LABWN*23, TITLE*100
C     !DASH
      external GILGIT, FERREX, FUROR, ARRADD, IOTA, MASHED, HI, BYE
C
      dimension W(*), IW(*)
C
C               WN(N,N), WH(N,N), TSHL(NRPMX,NSHL), WHD(N,N), WHS(N,N),
      dimension WN(*),   WH(*),   TSHL(*),          WHD(*),   WHS(*),
C
C               WSHL(N,NSHL), CDSK(N,MRR), CODSRW(NSHL), CSHL(N,NSHL),
     $          WSHL(*),      CDSK(*),     CODSRW(*),    CSHL(*),
C
C               WDSK(N,MRR), WND(N,N), WNS(N,N), TDSK(N,MRR)
     $          WDSK(*),     WND(*),   WNS(*),   TDSK(*)
C
      data LABWN /'Total WN (Shell + Disk)'/
      data LABWH /'Total WH (Shell + Disk)'/
C     !EJECT
C
      call HI ('HELIOS')
C     !BEG
      N2 = N**2
C
C---- Set up debug output unit, and print header
      call GILGIT   (NSHL, MRR, TOPT, TITLE, 'HELIOS', DUMP)
C---- Compute Shell component
      call FERREX   (N, NSHL, NRPMX, TSHL, CSHL, CODSRW, Y, MOVING,
     $               WNS, WHS, WSHL, ILFLX, W, IW, TITLE, DUMP)
C---- Compute Disk component
      call FUROR    (N, MRR,         TDSK, CDSK,         Y, MOVING,
     $               WND, WHD, WDSK, ILFLX, W, IW, TITLE, DUMP)
C---- Add component WN matrices
      call ARRADD   (WNS, WND, WN, N2)
      if(DUMP) then
        call IOTA   (WN, N, TITLE, LABWN)
      end if
C
      if(ILFLX.gt.0) then
        call ARRADD (WHS, WHD, WH, N2)
        if(DUMP) then
          call IOTA (WH, N, TITLE, LABWH)
        end if
      end if
C
      if(DUMP) then
        call MASHED ('HELIOS')
      end if
C     !END
      call BYE ('HELIOS')
C
      return
      end
