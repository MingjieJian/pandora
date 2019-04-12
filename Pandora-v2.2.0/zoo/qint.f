      subroutine QINT
     $(XT,INCX,FT,INCF,NT,J,X,F,GOOD,JS,C)
C
C     Rudolf Loeser, 2006 Dec 08
C---- Cubic interpolation, for CUBINT.
C     J is such that XT(J) .lt. X .lt. XT(J+1).
C
C     See remarks in CUBINT concerning the structure of XT and FT.
C     (QINT is based on subroutine PINT.)
C     !DASH
      save
C     !DASH
      real*8 C, DET, E, F, FT, ONE, T, TEMP, THREE, TWO, X, XT, ZERO
      integer IA, IB, IC, ID, INCF, INCX, INDX, J, JA, JB, JC, JD, JS,
     $        KODE, NT
      logical GOOD
C     !DASH
      external INVERS, DIVVY
C
      dimension XT(*), FT(*), C(*)
      dimension E(4,4), TEMP(4,4), T(4), INDX(4)
C
      data ZERO,ONE,TWO,THREE /0.D0, 1.D0, 2.D0, 3.D0/
      data KODE /0/
C
C     !BEG
      GOOD = .false.
C     !EJECT
      if((J.gt.1).and.(J.lt.(NT-1))) then
C
        if(J.ne.JS) then
          JS = J
          IA = 1+INCX*(J-2)
          IB = IA+INCX
          IC = IB+INCX
          ID = IC+INCX
          JA = 1+INCF*(J-2)
          JB = JA+INCF
          JC = JB+INCF
          JD = JC+INCF
C
          E(1,1) = ONE
          E(2,1) = ONE
          E(3,1) = ZERO
          E(4,1) = ZERO
          E(1,2) = XT(IB)
          E(2,2) = XT(IC)
          E(3,2) = ONE
          E(4,2) = ONE
          E(1,3) = E(1,2)*XT(IB)
          E(2,3) = E(2,2)*XT(IC)
          E(3,3) = E(1,2)*TWO
          E(4,3) = E(2,2)*TWO
          E(1,4) = E(1,3)*XT(IB)
          E(2,4) = E(2,3)*XT(IC)
          E(3,4) = E(1,3)*THREE
          E(4,4) = E(2,3)*THREE
C
          call INVERS (E, 4, 4, DET, 0, KODE, INDX, TEMP)
          if(KODE.eq.1) then
            T(1) = FT(JB)
            T(2) = FT(JC)
            call DIVVY ((FT(JC)-FT(JA)), (XT(IC)-XT(IA)), T(3))
            call DIVVY ((FT(JD)-FT(JB)), (XT(ID)-XT(IB)), T(4))
            C(1) = E(1,1)*T(1) +E(1,2)*T(2) +E(1,3)*T(3) +E(1,4)*T(4)
            C(2) = E(2,1)*T(1) +E(2,2)*T(2) +E(2,3)*T(3) +E(2,4)*T(4)
            C(3) = E(3,1)*T(1) +E(3,2)*T(2) +E(3,3)*T(3) +E(3,4)*T(4)
            C(4) = E(4,1)*T(1) +E(4,2)*T(2) +E(4,3)*T(3) +E(4,4)*T(4)
          else
            JS = -1
          end if
        end if
C
        if(KODE.eq.1) then
          F = C(1)+X*(C(2)+X*(C(3)+X*C(4)))
          GOOD = .true.
        end if
C
      end if
C     !END
C
      return
      end
