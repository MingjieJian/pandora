      subroutine FIXIT
     $(N,NMX,X,F,FUNC,VEC,XMIN,XMAX,DXMIN,DXMAX,RACC,CUT)
C
C     Rudolf Loeser, 1984 Jul 05
C---- Given the function F(X) = FUNC (X,VEC), this routine
C     sets up a table of N values of X. These values
C     of X are intended to yield a piecewise linear
C     approximation of F(X) to the specified tolerance RACC.
C     X(1) .eq. XMIN, X(N) .le. XMAX.
C     The corresponding values of F are also returned.
C---- There may be at most NMX values of X (and F).
C     If FIXIT would have established more than NMX such values,
C     then it returns with N=0, and with X and F
C     containing NMX values each; (in this case, X(NMX) will
C     generally not be the required maximum value).
C     !DASH
      save
C     !DASH
      real*8 CUT, DX, DXMAX, DXMIN, F, FA, FB, FC, FEX, FM, FMX, HALF,
     $       R, RACC, TWO, VEC, X, XA, XB, XC, XMAX, XMIN, XMXR, ZERO
      integer N, NMX
C     !DASH
      external  FUNC, DIVVY
      intrinsic abs, max, min
C
      dimension X(*), F(*), VEC(*)
C
      data ZERO,HALF,TWO /0.D0, 0.5D0, 2.0D0/
C
C     !BEG
C---- Initialize
      XMXR = XMAX
      DX   = DXMIN
      X(1) = XMIN
      call FUNC (X(1),VEC,F(1))
      FMX = abs(F(1))
      N   = 1
C     !EJECT
  100 continue
C----   Set up current base point
        XA = X(N)
        FA = F(N)
  101   continue
C----     March along with current DX
          XB = min((XA+DX),XMXR)
          call FUNC  (XB,VEC,FB)
          XC = min((XB+DX),XMXR)
          call FUNC  (XC,VEC,FC)
          FMX = max(abs(FB),abs(FC),FMX)
C----     Compute FEX (and FM)
          call DIVVY ((FB*(XC-XA)-FA*(XC-XB)),(XB-XA),FEX)
          FM = max(abs(FEX),abs(FC))
C----     (Adjust XMXR, the stopping point)
          if(FM.lt.CUT*FMX) then
            XMXR = min((XC+DX),XMXR)
          end if
C----     Compute and test R
          if(FM.eq.ZERO) then
            R = ZERO
          else
            R = abs(FEX-FC)/FM
          end if
          if(R.lt.RACC) then
C----       Good result - increase DX for next time around,
C           and accept current result
            DX = min((TWO*DX),DXMAX)
          else
C----       Bad result - decrease DX if possible
            DX = HALF*DX
            if(DX.ge.DXMIN) then
C----         DX is still usable - try again with current base point
              goto 101
            else
C----         DX too small - stick with minimum now,
C             and accept current result
              DX = DXMIN
            end if
          end if
C----     Accept current result (if there is room for it)
          if(N.lt.NMX) then
C----       Yes, ...
            N = N+1
            X(N) = XB
            F(N) = FB
C----       ... and step along to next base point
            if(XB.lt.XMXR) goto 100
          else
C----       No - set error signal, and fall out
            N = 0
          end if
C     !END
C
      return
      end
