      subroutine GENGHIS
     $(TMU,N,CMU,LG,FIN,Y,MOVING,WN,WMU,WH,ILFLX,WNM,WHM,TAU,W,IW,DUMP)
C
C     Rudolf Loeser, 1981 May 07
C---- Computes GR-weight matrices by approximating the angle-integral
C     with the trapezoidal rule.
C     !DASH
      save
C     !DASH
      real*8 CMU, TAU, TMU, W, WH, WHM, WMU, WN, WNM, Y, dummy
      integer ILFLX, IW, J, LG, N, N2, jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED
      character qummy*8
C     !DASH
      external ZERO1, ARGUN, LAMBDA, PHI, SAMOSIR, GURAN, HI, BYE
C
      dimension W(*), IW(*)
C
C               WH(N,N), TAU(N,N), CMU(LG), WN(N,N), WNM(N,N), WMU(LG),
      dimension WH(*),   TAU(*),   CMU(*),  WN(*),   WNM(*),   WMU(*),
C
C               WHM(N,N), TMU(N,LG)
     $          WHM(*),   TMU(N,*)
C
      data TAURED /.true./
      data GDIL   /.false./
C
      call HI ('GENGHIS')
C     !BEG
C---- Initialize
      N2 = N**2
      call ZERO1   (WN, N2)
      if(ILFLX.gt.0) then
        call ZERO1 (WH, N2)
      end if
C     !EJECT
C---- Loop over angles
      do 100 J = 1,LG
C
C----   Set up TAUs for this angle
        call ARGUN     (N, N, TMU(1,J), MOVING, TAU)
C
C----   Compute WNM for this angle (and dump ?)
        call LAMBDA    (dummy, W, IW, TAU, N, N, Y, FIN, TAURED, GDIL,
     $                  jummy, qummy, WNM)
        if(DUMP) then
          call GURAN   (WNM, N, 'WN', J)
        end if
C
C----   Add (i.e. do integration over angle)
        call SAMOSIR   (WN, WNM, CMU(J), N)
C
        if(ILFLX.gt.0) then
C----     Compute WHM (and dump ?)
C         (done here because it too needs TAU)
          call PHI     (TAU, N, N, Y, FIN, WHM, W)
          if(DUMP) then
            call GURAN (WHM, N, 'WH', J)
          end if
C
C         Add
          call SAMOSIR (WH, WHM, WMU(J), N)
        end if
C
  100 continue
C     !END
      call BYE ('GENGHIS')
C
      return
      end
