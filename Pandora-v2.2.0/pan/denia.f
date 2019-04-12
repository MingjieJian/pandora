      subroutine DENIA
     $(Y,IONST,F)
C
C     Rudolf Loeser, 1992 Jan 14
C---- Computes F, for NIOBE.
C     (This is version 2 of DENIA.)
C     !DASH
      save
C     !DASH
      real*8 E1, EX, F, FF, RT, W, Y, YY
      integer IONST, IRET, J, JS
C     !DASH
      external  EXPINT, PARINT, HI, BYE
      intrinsic min, max
C
      dimension YY(11), FF(11,2), W(4)
C
      data YY /
     $ 5.D-3, 1.D-2, 2.D-2, 4.D-2, 1.D-1, 2.D-1, 4.D-1, 1.D0, 2.D0,
     $ 4.D0, 1.D1/
C
      data FF /
     $ 2.76D-1, 1.16D0, 9.56D-1, 7.58D-1, 4.93D-1, 3.31D-1, 2.09D-1,
     $ 1.D-1, 6.3D-2, 4.D-2, 2.3D-2,
     $ 2.76D-1, 1.16D0, 9.77D-1, 7.88D-1, 5.54D-1, 4.03D-1, 2.9D-1,
     $ 2.14D-1, 2.01D-1, 2.D-1, 2.D-1/
C
      data JS /0/
C
      call HI ('DENIA')
C     !BEG
      J = max((min(IONST,2)),1)
C
      if(Y.le.YY(1)) then
        call EXPINT  (1, Y, E1, EX)
        F = FF(1,J)*E1
C
      else if(Y.ge.YY(11)) then
        if(J.eq.1) then
          RT = sqrt(YY(11)/Y)
          F  = FF(11,J)*RT
        else
          F = FF(11,J)
        end if
C
      else
        call PARINT  (YY, 1, FF(1,J), 1, 11, Y, F, 1, IRET, JS, W)
      end if
C     !END
      call BYE ('DENIA')
C
      return
      end
