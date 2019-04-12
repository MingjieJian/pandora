      subroutine BIRCH
     $(W,N,I,JL,DT,SE2,SE3,SES2,SES3)
C
C     Rudolf Loeser, 1981 Mar 23 (revised 2000 Jan 25)
C---- Computes terms to the left of the diagonal, for RT weight
C     matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SES2, SES3, TERM, W
      integer I, JL, KODE, N
C     !DASH
      external  LR, LL, HI, BYE
C
C               SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N), DT(2*N),
      dimension SE2(*),   SE3(*),   SES2(*),   SES3(*),   DT(*),
C
C               W(N,N)
     $          W(N,*)
C
      call HI ('BIRCH')
C     !BEG
  100 continue
C
        if(JL.gt.1) then
          call LR   (DT,JL,TERM,SE2,SE3,SES2,SES3,KODE)
          if(KODE.gt.0) then
            W(I,JL) = W(I,JL)+TERM
C
            JL = JL-1
            call LL (DT,JL,TERM,SE2,SE3,SES2,SES3,KODE)
            if(KODE.gt.0) then
              W(I,JL) = W(I,JL)+TERM
              goto 100
            end if
C
          end if
        end if
C
      continue
C     !END
      call BYE ('BIRCH')
C
      return
      end
