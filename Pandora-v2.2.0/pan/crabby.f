      subroutine CRABBY
     $(INDX,WVL,N,NOPAC,TE,CHN,CO)
C
C     Rudolf Loeser, 2006 Dec 27
C---- Computes CH bound-free absorption.
C     !DASH
      save
C     !DASH
      real*8 CHN, CO, CROSS, TE, WVL
      integer INDX, J, N, NOPAC
      logical FOK, lummy
C     !DASH
      external CHCROSS, ZEROD, HI, BYE
C
C               CO(Nopac,N), CHN(N), TE(N)
      dimension CO(NOPAC,*), CHN(*), TE(*)
C
      call HI ('CRABBY')
C     !BEG
      call CHCROSS     (WVL, TE(1), FOK, lummy, CROSS)
      if(FOK) then
        do 100 J = 1,N
          call CHCROSS (WVL, TE(J), FOK, lummy, CROSS)
          CO(INDX,J) = CHN(J)*CROSS
  100   continue
      else
        call ZEROD     (CO(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('CRABBY')
C
      return
      end
