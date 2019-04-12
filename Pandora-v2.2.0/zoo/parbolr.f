      subroutine PARBOLR
     $(X,INCX,Y,INCY,J,D,F,IRET)
C     Rudolf Loeser, 1992 Aug 07
C     (This is version 2 of PARBOIL)
C     (Originally written for CDC 6400, 1972 Jan 24)
C     This is a common subroutine for HELENAR and PARINTR
C     for real*4 operands.
C
C---- It decides which parabola to use in treating the interval
C     X(i-1) to X(i).
C
C---- Successive elements of "X" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "X" lives in X(II), where II=1+INCX*(I-1).
C
C---- Successive elements of "Y" are stored in memory locations
C     separated by the constant stride INCY, INCY > 0, such that the
C     I'th element of "Y" lives in Y(II), where II=1+INCY*(I-1).
C
C---- Upon return,
C     IRET=3 means: use upper parabola;
C     IRET=2      : use straight line;
C     IRET=1      : use lower parabola.
C
C---- It also returns further intermediate results in
C     arrays D and F.
C     !DASH
      save
C     !DASH
      real*4 D, D13, D14, D24, F, S12, S23, S34, X, Y, ZERO
      integer INCX, INCY, IRET, J, JX, JY
C     !DASH
      intrinsic abs
C
      dimension X(*), Y(*), D(3), F(3)
C
      data ZERO /0.E0/
C     !EJECT
C
C     !BEG
      JX = 1+INCX*(J-1)
      JY = 1+INCY*(J-1)
      D(1) = X(JX       )-X(JX-INCX)
      F(1) = Y(JY       )-Y(JY-INCY)
      D(2) = X(JX+  INCX)-X(JX     )
      F(2) = Y(JY+  INCY)-Y(JY     )
      D(3) = X(JX+2*INCX)-X(JX+INCX)
      F(3) = Y(JY+2*INCY)-Y(JY+INCY)
      S12 = F(1)/D(1)
      S23 = F(2)/D(2)
      S34 = F(3)/D(3)
      D24 = S23-S34
      D13 = S23-S12
      D14 = abs(S12)-abs(S34)
      if(D13.lt.ZERO) then
        if(D24.lt.ZERO) then
          IRET = 2
        else
          if(S12.le.ZERO) then
            IRET = 1
          else
            if(S34.ge.ZERO) then
              IRET = 3
            else
              if(D14.ge.ZERO) then
                IRET = 3
              else
                IRET = 1
              end if
            end if
          end if
        end if
      else
        if(D24.gt.ZERO) then
          IRET = 2
        else
          if(S34.le.ZERO) then
            IRET = 3
          else
            if(S12.ge.ZERO) then
              IRET = 1
            else
              if(D14.le.ZERO) then
                IRET = 1
              else
                IRET = 3
              end if
            end if
          end if
        end if
      end if
C     !END
C
      return
      end
