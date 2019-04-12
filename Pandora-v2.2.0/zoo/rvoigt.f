      subroutine RVOIGT
     $(X,A,VOIGT)
C
C     Algorithm by   G e o r g e    R y b i c k i .
C
C---- Computes the Voigt function to high precision
C     for any value of X and any positive value of A.
C
C     Restructured and recoded by R. Loeser.
C
C     !DASH
      save
C     !DASH
      real*8 A, A1, A2, ARG, B1, B2, C, DPS, EA, EX, F0, F01, F025, F05,
     $       F1, F15, F2, F225, F3, F9, FSM, R1, R2, R3, R4, SA, SD, SU,
     $       TD, TU, VOIGT, VV, W, WD, WU, X, X2, XX, YY, ZI, ZR
      integer N
      logical KILROY
C     !DASH
      external COMEXPD, COMCOSD
C
      dimension C(16)
C
      data KILROY /.true./
      data R1,R2 /9.42477796076938D+0, 5.64189583547756D-1/
      data R3,R4 /2.960881320326805D+1, 8.97935610625833D-2/
      data F0,F01,F025,F05 /0.D0, 1.D-1, 2.5D-1, 5.D-1/
      data F1,F15,F2,F225 /1.D0, 1.5D0, 2.D0, 2.25D0/
      data F3,F9 / 3.D0, 9.D0/
      data DPS /2.5D-12/
C
C     !BEG
      if(KILROY) then
C----   Initialization
        KILROY = .false.
        do 100 N = 1,16
          ARG  = (N-1)**2
          EA   = exp(-ARG/F9)
          C(N) = R4*EA
  100   continue
      end if
      X2 = X**2
      if(A.eq.F0) then
C----   Calculation for A=0
        EX = exp(-X2)
        VV = R2*EX
      else
C     !EJECT
C----   Calculation for the general case
        A1  = F3*A
        A2  = A**2
        FSM = A*R3
        if(A.ge.F01) then
          XX = -R1*A
          YY =  R1*X
          call COMEXPD (XX,YY,ZR,ZI)
          VV = F0
        else
          XX = R1*X
          YY = R1*A
          call COMCOSD (XX,YY,ZR,ZI)
          EX = exp(A2-X2)
          SA = cos(F2*A*X)
          VV = R2*EX*SA
        end if
        B1 =  (F1-ZR)*A*F15
        B2 = -ZI
        SU = -F15*X
        TU =  F225*(X2+A2)
        VV =  VV+C(1)*(B1+B2*SU)/TU
        SD =  SU
        TD =  TU
        do 101 N = 2,16
          B1 =  A1-B1
          B2 = -B2
          TU =  TU+SU+F025
          SU =  SU+F05
          if(TU.le.DPS) then
            WU = -C(N)*FSM
          else
            WU =  C(N)*(B1+B2*SU)/TU
          end if
          SD = SD-F05
          TD = TD-SD-F025
          if(TD.le.DPS) then
            WD = -C(N)*FSM
          else
            WD =  C(N)*(B1+B2*SD)/TD
          end if
          W  = WU+WD
          VV = VV+W
  101   continue
      end if
      VOIGT = VV
C     !END
C
      return
      end
