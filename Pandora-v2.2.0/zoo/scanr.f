      subroutine SCANR
     $(A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,KLT,KEQ,KGT,KXCL)
C
C     Rudolf Loeser, 1979 Apr 19
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*4 A, DELTA, DIV, XCL, ZERO
      integer FLAG, I, IMAX, IMIN, INC, J, JMN, JMX, KEQ, KGT, KLT,
     $        KXCL, N
      logical KILROY
C     !DASH
      external COMPR
C
      dimension A(*)
C
      data ZERO /0./
C
C     !BEG
      IMIN   = 1
      IMAX   = 1
      KILROY = .true.
      KLT  = 0
      KEQ  = 0
      KGT  = 0
      KXCL = 0
      JMN  = 0
      JMX  = 0
C     !EJECT
      J = 1-INC
      do 100 I = 1,N
        J = J+INC
        call COMPR     (A(J), XCL, ZERO, FLAG)
        if(FLAG.eq.0) then
          KXCL = KXCL+1
        else
          if(KILROY) then
            KILROY = .false.
            IMIN = I
            JMN  = J
            IMAX = I
            JMX  = J
          else
            call COMPR (A(J), A(JMN), DELTA, FLAG)
            if(FLAG.lt.0) then
              IMIN = I
              JMN  = J
            end if
            call COMPR (A(J), A(JMX), DELTA, FLAG)
            if(FLAG.gt.0) then
              IMAX = I
              JMX  = J
            end if
          end if
          call COMPR   (A(J), DIV, DELTA, FLAG)
          if(FLAG.lt.0) then
            KLT = KLT+1
          else if(FLAG.gt.0) then
            KGT = KGT+1
          else
            KEQ = KEQ+1
          end if
        end if
  100 continue
C     !END
C
      return
      end
