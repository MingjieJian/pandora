      subroutine WHORL
     $(N,B,CNXP,R,IQINC,BB)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Computes BB, for SASKIA.
C     !DASH
      save
C     !DASH
      real*8 B, BB, CNXP, R
      integer IQINC, N
C     !DASH
      external MOVE1, MULSUM, HI, BYE
C
C               B(N), CNXP(N), R(N), BB(N)
      dimension B(*), CNXP(*), R(*), BB(*)
C
      call HI ('WHORL')
C     !BEG
      call MOVE1    (B,N,BB)
      if(IQINC.gt.0) then
        call MULSUM (R,CNXP,BB,N)
      end if
C     !END
      call BYE ('WHORL')
C
      return
      end
