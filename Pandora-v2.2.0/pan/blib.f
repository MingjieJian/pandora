      subroutine BLIB
     $(LU,P,NE)
C
C     Rudolf Loeser, 2002 Apr 17
C---- Dumps, for DURIAN.
C     !DASH
      save
C     !DASH
      real*8 P
      integer LU, NE
C     !DASH
      external ARROUT, HI, BYE
C
C               P(NE,NE)
      dimension P(*)
C
      call HI ('BLIB')
C     !BEG
      if(LU.gt.0) then
        call ARROUT (LU,P,NE,NE,'P')
      end if
C     !END
      call BYE ('BLIB')
C
      return
      end
