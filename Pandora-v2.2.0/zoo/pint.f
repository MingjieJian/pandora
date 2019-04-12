      subroutine PINT
     $(XT,INCX,FT,INCF,NT,J,X,F,GOOD,JS,R)
C
C     Rudolf Loeser, 1981 Feb 25
C---- Parabolic interpolation, for PARINT.
C     J is such that XT(J) .lt. X .lt. XT(J+1).
C
C     See remarks in PARINT concerning the structure of XT and FT.
C     !DASH
      save
C     !DASH
      real*8 D, F, FT, G, R, TAB, X, XT, Z, ZERO
      integer INCF, INCX, IRET, J, JF, JS, JX, K, KF, KX, M, MTAB, NT
      logical GOOD
C     !DASH
      external PARBOIL
C
      dimension XT(*), FT(*), R(4), TAB(3), MTAB(3), D(3), G(3)
C
      data ZERO /0.D0/
      data TAB  /-1.D0, 0.D0, +1.D0/
      data MTAB /1, 0, 2/
C
C     !BEG
      GOOD = .false.
      if((J.gt.1).and.(J.lt.(NT-1))) then
        if(J.ne.JS) then
          JS = J
          call PARBOIL (XT, INCX, FT, INCF, J, D, G, IRET)
          R(4) = TAB(IRET)
          M    = MTAB(IRET)
          if(M.ne.0) then
            R(1) = (D(M)**2)*G(M+1)+(D(M+1)**2)*G(M)
            R(2) = D(M)*G(M+1)-D(M+1)*G(M)
            R(3) = D(M)*D(M+1)*(D(M+1)+D(M))
          end if
        end if
        if(R(4).ne.ZERO) then
          if(R(4).lt.ZERO) then
            K = J
          else
            K = J+1
          end if
          KX   = 1+INCX*(K-1)
          KF   = 1+INCF*(K-1)
          Z    = X-XT(KX)
          F    = FT(KF)+Z*(R(1)+Z*R(2))/R(3)
          GOOD = .true.
        end if
      end if
C     !END
C
      return
      end
