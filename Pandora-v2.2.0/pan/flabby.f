      subroutine FLABBY
     $(INDX,WVL,N,NOPAC,TE,OHN,CO)
C
C     Rudolf Loeser, 2006 Dec 27
C---- Computes OH bound-free absorption.
C     !DASH
      save
C     !DASH
      real*8 CO, CROSS, OHN, TE, WVL
      integer INDX, J, N, NOPAC
      logical FOK, lummy
C     !DASH
      external OHCROSS, ZEROD, HI, BYE
C
C               CO(Nopac,N), OHN(N), TE(N)
      dimension CO(NOPAC,*), OHN(*), TE(*)
C
      call HI ('FLABBY')
C     !BEG
      call OHCROSS     (WVL, TE(1), FOK, lummy, CROSS)
      if(FOK) then
        do 100 J = 1,N
          call OHCROSS (WVL, TE(J), FOK, lummy, CROSS)
          CO(INDX,J) = OHN(J)*CROSS
  100   continue
      else
        call ZEROD     (CO(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('FLABBY')
C
      return
      end
