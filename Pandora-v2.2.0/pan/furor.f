      subroutine FUROR
     $(N,MRR,TDSK,CDSK,Y,MOVING,WND,WHD,WDSK,ILFLX,W,IW,TITLE,DUMP)
C
C     Rudolf Loeser, 1983 Mar 15
C---- Computes the "Disk weight matrices", for HELIOS.
C     (This is version 3 of FUROR.)
C     !DASH
      save
C     !DASH
      real*8 CDSK, TDSK, W, WDSK, WHD, WND, Y, dummy
      integer ILFLX, IN, IS, ITAU, IW, IWHZ, IWNZ, J, MOX, MRR, N, N2,
     $        jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED
      character LAB*8, LABWH*32, LABWN*32, TITLE*(*), qummy*8
C     !DASH
      external ZERO1, LAMBDA, ERLIK, TOBA, JULIA, IOTA, ARGUN, PHI,
     $         WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               TDSK(N,MRR), CDSK(N,MRR), WDSK(N,MRR), WHD(N,N),
      dimension TDSK(N,*),   CDSK(N,*),   WDSK(N,*),   WHD(*),
C
C               WND(N,N)
     $          WND(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWNZ  ),(IN( 2),IWHZ  ),(IN( 3),ITAU  )
C
      data FIN    /.false./
      data TAURED /.true./
      data GDIL   /.false./
C
      data LABWN /' **********   Integrated WN/Disk'/
      data LABWH /' **********   Integrated WH/Disk'/
C     !EJECT
C
      call HI ('FUROR')
C     !BEG
C     (Get, and allocate, W allotment)
      call JULIA     (IN,IS,MOX,'FUROR')
C
      N2 = N**2
      call ZERO1     (WND, N2)
      if(ILFLX.gt.0) then
        call ZERO1   (WHD, N2)
      end if
C
C---- Loop over all rays
      do 101 J = 1,MRR
C----   Set up TAUs for this ray
        call ARGUN   (N, N, TDSK(1,J), MOVING, W(ITAU))
C----   Compute WNZ
        call LAMBDA  (dummy, W, IW, W(ITAU), N, N, Y, FIN, TAURED,
     $                GDIL, jummy, qummy, W(IWNZ))
C----   Add to WND
        call TOBA    (WND, W(IWNZ), CDSK(1,J), N, N)
C
        if(ILFLX.gt.0) then
C----     Compute WHD
C         (done here because it too needs TAU)
          call PHI   (W(ITAU), N, N, Y, FIN, W(IWHZ), W)
C----     Add to WHD
          call TOBA  (WHD, W(IWHZ), WDSK(1,J), N, N)
        end if
C
        if(DUMP) then
          write (LAB,100) J
  100     format('Disk ',I3)
          call ERLIK (LAB, N, TDSK(1,J), W(IWNZ), CDSK(1,J), W(IWHZ),
     $                WDSK(1,J), ILFLX)
        end if
  101 continue
C
      if(DUMP) then
        call IOTA    (WND, N, TITLE, LABWN)
        if(ILFLX.gt.0) then
          call IOTA  (WHD, N, TITLE, LABWH)
        end if
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'FUROR')
C     !END
      call BYE ('FUROR')
C
      return
      end
