      subroutine XAVIER
     $(X,W,N,Z,F,R,LABEL,IMG)
C
C     Rudolf Loeser, 2006 Dec 19
C---- Computes R = integral(F), for H.S.E.
C     !DASH
      save
C     !DASH
      real*8 F, R, W, X, Z
      integer IMG, LGT, N
      logical PARA
      character LABEL*100
C     !DASH
      external PLUSD, JAMES, BUSH, HURT, HI, BYE
C
      dimension X(*), W(*)
C
C               Z(N), F(N), R(N), IMG(N)
      dimension Z(*), F(*), R(*), IMG(*)
C
      data PARA /.true./
C
      call HI ('XAVIER')
C     !BEG
      if(PARA) then
C----   Use Simpson's Rule, choosing the "better" parabola
        call PLUSD   (F, 1, N, LGT)
        if(LGT.eq.N) then
C----     Compute integral elaborately, using logs
          call JAMES (X, W, N, Z, F, R, LABEL, IMG)
        else
C----     Compute integral simply
          call BUSH  (Z, 1, F, 1, R, 1, N)
        end if
      else
C----   Use Trapezoidal Rule
        call HURT    (N, Z, F, R)
      end if
C     !END
      call BYE ('XAVIER')
C
      return
      end
