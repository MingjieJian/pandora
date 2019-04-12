      subroutine ARAWN
     $(N,I,IMX,IDM,NRP,XRAY,OPAC,LABEL,TOPT,MM,WN,KODE,TAUK,TAUW,OPAW,
     $ WNR,WH,WHR,Y,MOVING,ILFLX,IMG,WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 08
C---- Computes weight matrices for a particular shell ray, for
C     angle-dependent continuum calculations.
C     !DASH
      save
C     !DASH
      real*8 OPAC, OPAW, TAUK, TAUW, W, WH, WHR, WN, WNR, WRK, XRAY, Y,
     $       dummy
      integer I, IDM, II, ILFLX, IMG, IMX, IW, KERR, KODE, MM, N, NRP,
     $        jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED, TOPT
      character LABEL*100, TITLE*100, qummy*8
C     !DASH
      external  DIDO, LAMBDA, NIDABA, TROYES, RAMIREZ, MOVED, PHI,
     $          HI, BYE
      intrinsic min
C
      dimension W(*), IW(*)
C
C               OPAC(N+3,NRPMX), WHR(NRPMX,NRPMX), XRAY(NRPMX), IMG(N),
      dimension OPAC(IDM,*),     WHR(*),           XRAY(*),     IMG(*),
C
C               WNR(NRPMX,NRPMX), TAUK(NRPMX,NRPMX), WH(N,N), WN(N,N),
     $          WNR(*),           TAUK(NRP,*),       WH(*),   WN(*),
C
C               TAUW(NRPMX), OPAW(NRPMX), WRK(NRPMX,NRPMX)
     $          TAUW(*),     OPAW(*),     WRK(*)
C
      data FIN    /.true./
      data TAURED /.true./
      data GDIL   /.false./
C     !EJECT
C
      call HI ('ARAWN')
C     !BEG
      KODE = 1
C
C---- Compute optical depth
      do 101 II = 1,IMX
        call MOVED     (OPAC(II,1), IDM, NRP, OPAW, 1, NRP)
C
        write (LABEL(51:75),100) II
  100   format('Optical Depth for I=',I5)
        call DIDO      (XRAY, OPAW, NRP, TAUW, KERR, TOPT, LABEL, IMG,
     $                  W)
        KODE = min(KODE,KERR)
C
        if(DUMP) then
          call RAMIREZ (II, 1, XRAY, OPAW, TAUW, NRP)
        end if
        call MOVED     (TAUW, 1, NRP, TAUK(II,1), NRP, NRP)
  101 continue
C
      if(KODE.eq.1) then
C----   Compute WNR
        call LAMBDA    (dummy, W, IW, TAUK, IMX, NRP, Y, FIN, TAURED,
     $                  GDIL, jummy, qummy, WNR)
        call NIDABA    (I, WNR, WN, W)
        if(ILFLX.gt.0) then
C----     Compute WHR
C         (done here because it too needs TAUK)
          call PHI     (TAUK, IMX, NRP, Y, FIN, WHR, W)
          call NIDABA  (I, WHR, WH, W)
        end if
C
        if(DUMP) then
          write (TITLE,102) MM,I,NRP
  102     format('Debug Data for ',I2,'. shell ray; (',2I4,').')
          call TROYES  (IMX, NRP, TAUK, I, WN, WH, ILFLX, TITLE, WRK)
        end if
C
      end if
C     !END
      call BYE ('ARAWN')
C
      return
      end
