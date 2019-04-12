      subroutine SCANC
     $(A,INC,N,DIV,XCL,IMIN,IMAX,KLT,KEQ,KGT,KXCL)
C
C     Rudolf Loeser, 1979 Apr 19
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer FLAG, I, IMAX, IMIN, INC, J, JMN, JMX, KEQ, KGT, KLT,
     $        KXCL, N
      logical KILROY
      character A*(*), DIV*(*), XCL*(*)
C     !DASH
      external COMPC
C
      dimension A(*)
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
        call COMPC     (A(J), XCL, FLAG)
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
            call COMPC (A(J), A(JMN), FLAG)
            if(FLAG.lt.0) then
              IMIN = I
              JMN  = J
            end if
            call COMPC (A(J), A(JMX), FLAG)
            if(FLAG.gt.0) then
              IMAX = I
              JMX  = J
            end if
          end if
          call COMPC   (A(J), DIV, FLAG)
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
