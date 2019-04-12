      subroutine GLAUCON
     $(PRINT,PCOUNT,P,C,SPACE,LINE)
C     Rudolf Loeser, 1979 Jan 29
C---- Sets up print elements and carriage-control characters,
C     for HECTOR.
C     !DASH
      save
C     !DASH
      integer LINE, PCOUNT, PRINT
      character C*1, P*1, Q1*1, QB*1, QD*1, QO*1, QP*1, QX*1, SPACE*1
C     !DASH
      intrinsic mod
C
      dimension P(3), C(3)
C
      data QO,QX,QP,QD,QB,Q1 /'O','X','+','$',' ','1'/
C
C     !BEG
      if(PRINT.lt.0) then
        P(1) = QO
        P(2) = QX
        P(3) = QP
        C(2) = QP
        C(3) = QP
        PCOUNT = 3
      else if(PRINT.eq.0) then
        P(1) = QD
        PCOUNT = 1
      else
        P(1) = QB
        PCOUNT = 1
      end if
      C(1) = QB
      if(LINE.le.1) then
        C(1) = SPACE
      else
        if(mod(LINE,5).eq.1) then
          C(1) = Q1
        end if
      end if
C     !END
C
      return
      end
