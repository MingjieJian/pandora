      subroutine EDOUARD
     $(XMASS,RM,M)
C
C     Rudolf Loeser, 1990 Oct 03
C---- Computes ion mass-ratios, for population ions.
C     (This is version 2 of EDOUARD.)
C     !DASH
      save
C     !DASH
      real*8 RM, XMASS
      integer J, M
C     !DASH
      external HI, BYE
C
C               XMASS(M), RM(M)
      dimension XMASS(*), RM(*)
C
      call HI ('EDOUARD')
C     !BEG
      do 100 J = 1,M
        RM(J) = XMASS(J)/(XMASS(1)+XMASS(J))
  100 continue
C     !END
      call BYE ('EDOUARD')
C
      return
      end
