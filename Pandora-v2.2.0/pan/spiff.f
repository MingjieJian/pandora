      subroutine SPIFF
     $(MODE,X,FNU)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Makes sure FNU is in Frequency Units, for SPUME.
C     !DASH
      save
C     !DASH
      real*8 FNU, X
      integer MODE
C     !DASH
      external ANGIE, HI, BYE
C
      call HI ('SPIFF')
C     !BEG
      if(MODE.eq.1) then
        FNU = X
      else if(MODE.eq.2) then
        call ANGIE (X,FNU)
      end if
C     !END
      call BYE ('SPIFF')
C
      return
      end
