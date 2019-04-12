      subroutine HELENA
     $(X,INCX,F,INCF,A,INCA,N,SUM)
C     Rudolf Loeser, 1981 Feb 24
C     (This is version 2 of HELENA)
C     (Originally written for CDC 6400, 1972 Jan 24)
C
C---- Given
C     a table of abscissae, X(i), 1 .le. i .le. N, and a corresponding
C     table of ordinates, F(i), 1 .le. i .le. N.
C
C---- "HELENA" computes a table of integrals such that
C     A(i) = integral of F(x) for the interval X(i-1) .le. x .le. X(i).
C     (A(1) always =0.)
C     Trapezoidal rule integration is used for A(2) and A(N), while
C     while either trapezoidal rule or parabolic integration is used
C     for all other intervals.
C     Parabolic interpolation uses the "better" of the two
C     possible parabolas.
C
C---- The table X must be in monotonic algebraic order, i.e.:
C     X(i) .ge. X(i-1), 2 .le. i .le. N; or
C     X(i) .le. X(i-1), 2 .le. i .le. N.
C
C---- Upon return, "SUM" = sum of all the A-S.
C
C---- Successive elements of "X" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "X" lives in X(II), where II=1+INCX*(I-1).
C
C---- Successive elements of "F" are stored in memory locations
C     separated by the constant stride INCF, INCF > 0, such that the
C     I'th element of "F" lives in F(II), where II=1+INCF*(I-1).
C
C---- Successive elements of "A" are stored in memory locations
C     separated by the constant stride INCA, INCA > 0, such that the
C     I'th element of "A" lives in A(II), where II=1+INCA*(I-1).
C     !DASH
      save
C     !DASH
      real*8 A, D, F, HALF, S, SIX, SUM, T1, T2, T3, T4, THREE, TWO, X,
     $       ZERO
      integer INCA, INCF, INCX, IRET, J, JA, JF, JX, N, NA, NF, NMO, NX
C     !DASH
C     !EJECT
      external  PARBOIL, ABORT
C
      dimension X(*), F(*), A(*), D(3), S(3)
C
      data ZERO,HALF,TWO,THREE,SIX /0.D0, 0.5D0, 2.D0, 3.D0, 6.D0/
C
C     !BEG
      if(N.gt.0) then
        NMO  = N-1
        A(1) = ZERO
        SUM  = ZERO
        if(N.gt.1) then
          NX = 1+INCX*(N-1)
          NF = 1+INCF*(N-1)
          NA = 1+INCA*(N-1)
          A(1+INCA) = HALF*(F(1+INCF)+F(1))*(X(1+INCX)-X(1))
          A(NA) = HALF*(F(NF)+F(NF-INCF))*(X(NX)-X(NX-INCX))
          SUM   = SUM+A(1+INCA)+A(NA)
          if(N.ge.3) then
            JX = 1+INCX
            JF = 1+INCF
            JA = 1+INCA
            do 101 J = 3,NMO
              JX = JX+INCX
              JF = JF+INCF
              JA = JA+INCA
              call PARBOIL (X,INCX,F,INCF,J-1,D,S,IRET)
              if(IRET.eq.2) then
                A(JA) = HALF*(F(JF)+F(JF-INCF))*D(2)
              else
                if(IRET.eq.3) then
                  T2 = X(JX-INCX)-X(JX+INCX)
                  T1 = T2**2
                  T3 = F(JF-INCF)-F(JF+INCF)
                  T4 = -T2*D(3)
                else if(IRET.eq.1) then
                  T2 = D(1)
                  T1 = T2**2
                  T3 = S(1)
                  T4 = (X(JX)-X(JX-2*INCX))*D(1)
                else
                  call ABORT
                  write (*,100) IRET
  100             format(' ','Error in HELENA:  IRET =',I12,
     $                       ', which is not 1, 2 or 3.')
                end if
                A(JA) = D(2)*(F(JF-INCF)+(THREE*T1*S(2)+
     $                  D(2)*(TWO*T2*S(2)+T3*D(2)))/(SIX*T4))
              end if
              SUM = SUM+A(JA)
  101       continue
          end if
        end if
      end if
C     !END
C
      return
      end
