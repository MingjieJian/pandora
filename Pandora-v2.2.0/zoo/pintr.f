      subroutine PINTR
     $(XT,INCX,FT,INCF,NT,J,X,FX,JS,R)
C     Rudolf Loeser, 1981 Feb 25
C---- Parabolic interpolation, for PARINTR.
C     J is such that XT(J) .lt. X .lt. XT(J+1).
C
C     See remarks in PARINTR concerning the structure of XT and FT.
C     !DASH
      save
C     !DASH
      real*4 D, F, FT, FX, R, TAB, X, XT, Z, ZERO
      integer INCF, INCX, IRET, J, JF, JS, JX, K, KF, KX, M, MTAB, NT
C     !DASH
      external PARBOLR, LINTR
C
      dimension XT(*), FT(*), R(4), TAB(3), MTAB(3), D(3), F(3)
C
      data ZERO /0.E0/
      data TAB /-1.E0, 0.E0, +1.E0/
      data MTAB /1, 0, 2/
C
C     !BEG
      JX = 1+INCX*(J-1)
      JF = 1+INCF*(J-1)
      if((J.gt.1).and.(J.LT.(NT-1))) then
        if(J.ne.JS) then
          JS = J
          call PARBOLR (XT,INCX,FT,INCF,J,D,F,IRET)
          R(4) = TAB(IRET)
          M    = MTAB(IRET)
          if(M.ne.0) then
            R(1) = (D(M)**2)*F(M+1)+(D(M+1)**2)*F(M)
            R(2) =  D(M)*F(M+1)-D(M+1)*F(M)
            R(3) =  D(M)*D(M+1)*(D(M+1)+D(M))
          end if
        end if
        if(R(4).ne.ZERO) then
          if(R(4).lt.ZERO) then
            K = J
          else
            K = J+1
          end if
          KX = 1+INCX*(K-1)
          KF = 1+INCF*(K-1)
          Z  = X-XT(KX)
          FX = FT(KF)+Z*(R(1)+Z*R(2))/R(3)
        else
          call LINTR   (XT(JX),FT(JF),XT(JX+INCX),FT(JF+INCF),1,X,FX)
        end if
      else
        call LINTR     (XT(JX),FT(JF),XT(JX+INCX),FT(JF+INCF),1,X,FX)
      end if
C     !END
C
      return
      end
