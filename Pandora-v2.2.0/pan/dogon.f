      subroutine DOGON
     $(W,N,WR,NR)
C
C     Rudolf Loeser, 1983 Mar 14
C---- Reduces the order of a weight matrix by one.
C     Note: NR = N-1.
C     (This is version 2 of DOGON.)
C     !DASH
      save
C     !DASH
      real*8 W, WR
      integer I, IR, J, JR, N, NR
C     !DASH
      external HI, BYE
C
C               W(N,N), WR(NR,NR)
      dimension W(N,*), WR(NR,*)
C
      call HI ('DOGON')
C     !BEG
      do 101 IR = 1,NR
C
        if(IR.lt.NR) then
          I = IR
        else
          I = IR+1
        end if
C
        do 100 JR = 1,NR
C
          if(JR.lt.NR) then
            J = JR
          else
            J = JR+1
          end if
C
          if(JR.eq.(NR-1)) then
            WR(IR,JR) = W(I,J)+W(I,J+1)
          else
            WR(IR,JR) = W(I,J)
          end if
C
  100   continue
  101 continue
C     !END
      call BYE ('DOGON')
C
      return
      end
