      subroutine QUINCE
     $(W,N,I,JR,DT,SE2,SE3,SES2,SES3)
C
C     Rudolf Loeser, 1981 Mar 23 (revised 2000 Jan 25)
C---- Computes terms to the right of the diagonal, for RT weight
C     matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SES2, SES3, TERM, W
      integer I, JR, KODE, N
C     !DASH
      external  RL, RR, HI, BYE
C
C               SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N), DT(2*N),
      dimension SE2(*),   SE3(*),   SES2(*),   SES3(*),   DT(*),
C
C               W(N,N)
     $          W(N,*)
C
      call HI ('QUINCE')
C     !BEG
  100 continue
C
        if(JR.lt.N) then
          call RL   (DT,JR,TERM,SE2,SE3,SES2,SES3,KODE)
          if(KODE.gt.0) then
            W(I,JR) = W(I,JR)+TERM
C
            JR = JR+1
            call RR (DT,JR,TERM,SE2,SE3,SES2,SES3,KODE)
            if(KODE.gt.0) then
              W(I,JR) = W(I,JR)+TERM
              goto 100
            end if
C
          end if
        end if
C
      continue
C     !END
      call BYE ('QUINCE')
C
      return
      end
