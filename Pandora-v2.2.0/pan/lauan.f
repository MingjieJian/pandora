      subroutine LAUAN
     $(TAU,IMAX,N,Y,WN,TITLE,KODE,W,IW,DUMP)
C
C     Rudolf Loeser, 1968 May 29
C---- LAUAN controls calculation of the QR weight matrix WN.
C     Upon return, KODE=1 if calculation was successful;
C                  KODE=0 if not.
C     !DASH
      save
C     !DASH
      real*8 TAU, W, WN, Y
      integer IFM, IGM, IMAX, IN, IS, IW, KODE, MOX, N
      logical DUMP
      character TITLE*(*)
C     !DASH
      external LUKE, LOCUST, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               WN(N,N), TAU(N)
      dimension WN(*),   TAU(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IGM   ),(IN( 2),IFM   )
C
      call HI ('LAUAN')
C     !BEG
C     (Get, and allocate, W allotment)
      call LUKE   (IN, IS, MOX, 'LAUAN', N)
C
      call LOCUST (TAU, IMAX, N, Y, WN, W(IGM), W(IFM), W, IW, DUMP,
     $             TITLE, KODE)
C
C     (Give back W allotment)
      call WGIVE  (W, 'LAUAN')
C     !END
      call BYE ('LAUAN')
C
      return
      end
