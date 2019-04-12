      subroutine LEOPOLD
     $(NO,IU,IL,XI,K)
C
C     Rudolf Loeser, 1992 Apr 13
C---- Prints a sketch of the distribution of XI values.
C     !DASH
      save
C     !DASH
      real*8 WB, XI, XL, XN, XR
      integer I, IL, IU, J, K, L, N, NO
      logical INBIN
      character LINE*106, PERIOD*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external WITHIN, HI, BYE
C
C               XI(K)
      dimension XI(*)
C
      data N,XN /107, 1.07D2/
C     !EJECT
C
      call HI ('LEOPOLD')
C     !BEG
      if(NO.gt.0) then
C----   Initialize bin data
        WB = (XI(K)-XI(1))/XN
        XR = XI(1)
C
C----   Initialize XI index
        J = 1
C
C----   Loop over all bins
        do 101 I = 1,N
C----     Set boundaries of current (Ith) bin...
          XL = XR
          XR = XR+WB
C         ...and initialize current bin count
          L = 0
C
  100     continue
C----       Is current (Jth) XI in current (Ith) bin?
            call WITHIN (XL,XI(J),XR,0,INBIN)
            if(INBIN) then
C----         Yes, it is; increase count...
              L = L+1
C             ...and step to next XI
              J = J+1
              if(J.le.K) go to 100
            end if
C
C----       Encode bin count for printing
            if(L.gt.35) then
              LINE(I:I) = STAR
            else if(L.gt.9) then
              LINE(I:I) = ALPHS(L-9)
            else if(L.gt.0) then
              LINE(I:I) = NUMBS(L+1)
            else
              LINE(I:I) = PERIOD
            end if
  101   continue
C
        write (NO,102) IU,IL,K,LINE(1:N)
  102   format(' ','XI(',I2,'/',I2,')',2X,'K =',I4,2X,A)
      end if
C     !END
      call BYE ('LEOPOLD')
C
      return
      end
