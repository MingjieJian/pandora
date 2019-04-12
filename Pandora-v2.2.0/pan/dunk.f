      subroutine DUNK
     $(INDEX,N,NOPAC,CO,B,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes those emission components that
C     = B * absorption component.
C     (This is version 2 of DUNK.)
C     !DASH
      save
C     !DASH
      real*8 B, CO, CONT
      integer INDEX, J, N, NOPAC
C     !DASH
      external HI, BYE
C
C               B(N), CO(Nopac,N), CONT(Nopac,N)
      dimension B(*), CO(NOPAC,*), CONT(NOPAC,*)
C
      call HI ('DUNK')
C     !BEG
      do 100 J = 1,N
        CONT(INDEX,J) = B(J)*CO(INDEX,J)
  100 continue
C     !END
      call BYE ('DUNK')
C
      return
      end
