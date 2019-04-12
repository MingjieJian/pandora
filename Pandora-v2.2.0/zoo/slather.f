      subroutine SLATHER
     $(X,F,N,CRIT,ITMX,INDX, DX,FP,DF,IM, KRET)
C     Rudolf Loeser, 1997 Feb 07
C     Revised, for standard PANDORA use, 1998 Oct 15.
C
C---- IMPROVED Sequential smoothing, irregular point spacing,
C     (cf. subroutine SMOTHER.)
C     (97 Jan 31,  Page 6 ff):
C
C     Upon return,
C     the smoothed values of F(X) replace the original values in F;
C     IM(i) will tell how many times F(i) was modified.
C
C     Returns with KRET .ge. 0 if all seems OK:
C                            the value is the number of changes made
C             with KRET .lt. 0 in case of error:
C                       = -1 if N .lt. 5
C                       = -2 if, for any i>1, X(i)-X(i-1) .le. 0.
C
C     DX, FP, DF, and IM are scratch storage arrays of length N.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DF, DX, F, FP, X
      integer I, IE, IM, IMX, INDX, IS, ITMX, J, K, KRET, L, ML, MR, N
      logical DUMP
C     !DASH
      external  ZEROI, BEEFY, SKINNY, STEEPLY
      intrinsic max, min
C
      dimension X(N), F(N), DX(N), FP(N), DF(N), IM(N)
C
C     !BEG
C---- Check X table for length and monotonicity
      call STEEPLY (X, DX, N, KRET)
      if(KRET.lt.0) then
        goto 102
      end if
C
C---- Initialize counters
      KRET = 0
      call ZEROI   (IM, 1, N)
C
C---- Initialize details dump controls
      DUMP = INDX.gt.0
      if(DUMP) then
        ML = max((INDX-5),1)
        MR = min((INDX+5),N)
      end if
C     !EJECT
      K = N*ITMX
      L = N-2
C
C---- Initialize FP and DF tables
      IS = 3
      IE = L
      call BEEFY      (IS, IE, DX, F, FP, DF)
C
      if(N.eq.5) then
C----   Special case: smallest fixable table
        if(FP(3).ne.F(3)) then
          F(3)  = FP(3)
          IM(3) = IM(3)+1
          KRET  = KRET+1
        end if
        goto 102
      end if
C
C---- General case - iterate til largest DF is small enough
      do 101 J = 1,K
C
C----   Find IMX, the index of the current largest DF
        IMX = 3
        do 100 I = 4,L
          if(DF(I).gt.DF(IMX)) then
            IMX = I
          end if
  100   continue
        if(DF(IMX).lt.CRIT) then
C----     This DF is small enough - quit
          goto 102
        end if
C
C----   Accept the "smoother/better" value at IMX
        F(IMX)  = FP(IMX)
        IM(IMX) = IM(IMX)+1
        KRET    = KRET+1
C
        if((IMX.ge.ML).and.(IMX.le.MR)) then
C----     Dump details in vicinity of IMX (to sysout)
          call SKINNY (ML, MR, IMX, INDX, X, DX, F, DF, FP, IM)
        end if
C
C----   Update FP and DF tables
        IS = max((IMX-2),3)
        IE = min((IMX+2),L)
        call BEEFY    (IS, IE, DX, F, FP, DF)
C
  101 continue
C
  102 continue
C     !END
C
      return
      end
