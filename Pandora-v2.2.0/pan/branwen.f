      subroutine BRANWEN
     $(N,XRAY,OPAC,LABEL,TAUK,TOPT,WN,KODE,MM,OPAW,TAUW,WH,Y,MOVING,
     $ ILFLX,IMG,WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 08
C---- Computes weight matrices for a particular disk ray,
C     for angle-dependent continuum calculations.
C     !DASH
      save
C     !DASH
      real*8 OPAC, OPAW, TAUK, TAUW, W, WH, WN, WRK, XRAY, Y, dummy
      integer I, ILFLX, IMG, IW, KERR, KODE, MM, N, jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED, TOPT
      character LABEL*100, TITLE*100, qummy*8
C     !DASH
      external  MOVED, DIDO, LAMBDA, TROYES, RAMIREZ, PHI, HI, BYE
      intrinsic min
C
      dimension W(*), IW(*)
C
C               OPAC(N,N), WRK(N,N), XRAY(N), WN(N,N), WH(N,N), IMG(N),
      dimension OPAC(N,*), WRK(*),   XRAY(*), WN(*),   WH(*),   IMG(*),
C
C               OPAW(N), TAUW(N), TAUK(N,N)
     $          OPAW(*), TAUW(*), TAUK(N,*)
C
      data FIN    /.false./
      data TAURED /.true./
      data GDIL   /.false./
C     !EJECT
C
      call HI ('BRANWEN')
C     !BEG
      KODE = 1
C
C---- Compute optical depth
      do 101 I = 1,N
        call MOVED     (OPAC(I,1), N, N, OPAW,1,N)
C
        write (LABEL(51:75),100) I
  100   format('Optical Depth for I=',I5)
        call DIDO      (XRAY, OPAW, N, TAUW, KERR, TOPT, LABEL, IMG, W)
        KODE = min(KODE,KERR)
        call MOVED     (TAUW, 1, N, TAUK(I,1), N, N)
C
        if(DUMP) then
          call RAMIREZ (I, 1, XRAY, OPAW, TAUW, N)
        end if
  101 continue
C
      if(KODE.eq.1) then
C----   Compute WN
        call LAMBDA    (dummy, W, IW, TAUK, N, N, Y, FIN, TAURED, GDIL,
     $                  jummy, qummy, WN)
        if(ILFLX.gt.0) then
C----     Compute WH
C         (done here because it too needs TAUK)
          call PHI     (TAUK, N, N, Y, FIN, WH, W)
        end if
C
        if(DUMP) then
          write (TITLE,102) MM
  102     format('Debug data for ',I2,'. disk ray.')
          call TROYES  (N, N, TAUK, N, WN, WH, ILFLX, TITLE, WRK)
        end if
C
      end if
C     !END
      call BYE ('BRANWEN')
C
      return
      end
