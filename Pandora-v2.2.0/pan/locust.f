      subroutine LOCUST
     $(TAU,IMAX,N,Y,WN,G,FI,W,IW,DUMP,TITLE,KODE)
C
C     Rudolf Loeser, 1968 May 29
C---- LOCUST computes QR weight matrix
C     for Source Function calculations.
C     Upon return, KODE=1 if all seems OK; =0 if not.
C     !DASH
      save
C     !DASH
      real*8 FI, G, TAU, W, WN, Y
      integer IMAX, IW, KODE, N
      logical DUMP
      character LABEL*18, TITLE*(*)
C     !DASH
      external LAUREL, REDBUD, CATALPA, IOTA, HI, BYE
C
      dimension W(*), IW(*)
C
C               TAU(N), WN(N,N), G(N,N), FI(N,N)
      dimension TAU(*), WN(*),   G(*),   FI(*)
C
      data LABEL /'Component matrix G'/
C
      call HI ('LOCUST')
C     !BEG
C---- Compute the G matrix
      call CATALPA  (TAU, IMAX, N, Y, G)
      if(DUMP) then
        call IOTA   (G, N, TITLE, LABEL)
      end if
C
C---- Compute the F-inverse matrix
      call LAUREL   (TAU, N, Y, FI, W, IW, DUMP, TITLE, KODE)
C
      if(KODE.eq.1) then
C----   Multiply the two to form WN
        call REDBUD (G, FI, WN, IMAX, N)
      end if
C     !END
      call BYE ('LOCUST')
C
      return
      end
