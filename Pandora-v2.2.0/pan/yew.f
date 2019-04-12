      subroutine YEW
     $(W,N,I,DT,SE2,SE3,SES2,SES3)
C
C     Rudolf Loeser, 1971 Jul 09 (revised 2000 Jan 25)
C---- Modifies the I-th row of a finite W-matrix - stepping through
C     DT to the right while stepping through the row to the left -
C     to make it apply to the reflective case, for the RT weight
C     matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SES2, SES3, TERM, W
      integer I, J, JR, KODE, N, NN
C     !DASH
      external  RL, RR, HI, BYE
C
C               DT(2*N), SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N),
      dimension DT(*),   SE2(*),   SE3(*),   SES2(*),   SES3(*),
C
C               W(N,N)
     $          W(N,*)
C
      call HI ('YEW')
C     !BEG
 
      NN = 2*N-1
      JR = N+1
      call RL         (DT,JR,TERM,SE2,SE3,SES2,SES3,KODE)
      if(KODE.gt.0) then
        J = N
        W(I,J) = W(I,J)+TERM
C
  100   continue
          call RR     (DT,JR,TERM,SE2,SE3,SES2,SES3,KODE)
          if(KODE.gt.0) then
            J = J-1
            W(I,J) = W(I,J)+TERM
C
            if(JR.lt.NN) then
              JR = JR+1
              call RL (DT,JR,TERM,SE2,SE3,SES2,SES3,KODE)
              if(KODE.gt.0) then
                W(I,J) = W(I,J)+TERM
                goto 100
              end if
            end if
C
          end if
        continue
C
      end if
C     !END
      call BYE ('YEW')
C
      return
      end
