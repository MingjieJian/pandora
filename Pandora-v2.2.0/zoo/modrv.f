      subroutine MODRV
     $(X,A,CRT,Z)
C     Original program written by   G e o r g e   R y b i c k i
C     (Algorithm restructured and recoded by R. Loeser)
C---- Computes the Voigt function
C     for any value of X and any positive value of A.
C
C     This is a version of RVOIGT which, using CRT, truncates the
C     summation to save execution time at the expense of precision.
C     !DASH
      save
C     !DASH
      real*4 A, A1, A2, ARG, B1, B2, C, CRT, EPS, F0, F01, F025, F05,
     $       F1, F15, F2, F225, F3, F9, FSM, Q1, Q2, Q3, Q4, SD, SU, TD,
     $       TU, VV, W, WD, WPART, WU, X, X2, XX, YY, Z, ZI, ZR
      integer N
      logical KILROY
C     !DASH
      external  COMEXP, COMCOS
      intrinsic abs, mod
C
      dimension C(17)
C
      data KILROY /.true./
      data Q1,Q2 /9.42477796076938E+0, 5.64189583547756E-1/
      data Q3,Q4 /2.960881320326805E+1, 8.97935610625833E-2/
      data F0,F01,F025,F05 /0.E0, 1.E-1, 2.5E-1, 5.E-1/
      data F1,F15,F2,F225 /1.E0, 1.5E0, 2.E0, 2.25E0/
      data F3,F9 / 3.E0, 9.E0/
      data EPS /2.5E-12/
C
C     !BEG
      if(KILROY) then
C----   Initialization
        KILROY=.false.
        do 100 N = 1,17
          ARG  = (N-1)**2
          C(N) = Q4*exp(-ARG/F9)
  100   continue
      end if
      X2 = X**2
      A2 = A**2
      if(A2.eq.F0) then
C----   Calculation for A=0
        VV = Q2*exp(-X2)
      else
C     !EJECT
C----   Calculation for the general case
        A1  = F3*A
        FSM = A*Q3
        if(A.ge.F01) then
          XX = -Q1*A
          YY =  Q1*X
          call COMEXP (XX,YY,ZR,ZI)
          VV = F0
        else
          XX = Q1*X
          YY = Q1*A
          call COMCOS (XX,YY,ZR,ZI)
          VV = Q2*exp(A2-X2)*cos(F2*A*X)
        end if
        B1 =  (F1-ZR)*A*F15
        B2 = -ZI
        SU = -F15*X
        TU =  F225*(X2+A2)
        VV =  VV+C(1)*(B1+B2*SU)/TU
        SD =  SU
        TD =  TU
        do 101 N = 2,17
          B1 =  A1-B1
          B2 = -B2
          TU =  TU+SU+F025
          SU =  SU+F05
          if(TU.le.EPS) then
            WU = -C(N)*FSM
          else
            WU =  C(N)*(B1+B2*SU)/TU
          end if
          SD = SD-F05
          TD = TD-SD-F025
          if(TD.le.EPS) then
            WD = -C(N)*FSM
          else
            WD =  C(N)*(B1+B2*SD)/TD
          end if
          W = WU+WD
          if(mod(N,2).eq.0) then
            WPART = W
          else
            W  = W+WPART
            VV = VV+W
            if(VV.eq.F0) goto 102
            if(abs(W/VV).le.CRT) goto 102
          end if
  101   continue
      end if
  102 continue
      Z = VV
C     !END
C
      return
      end
