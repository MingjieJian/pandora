      subroutine TRAVE
     $(KILROY,LU)
C
C     Rudolf Loeser, 1995 May 30
C---- Prints an optional heading for AMALFI.
C     !DASH
      save
C     !DASH
      integer LU
      logical KILROY
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('TRAVE')
C     !BEG
      if(LU.gt.0) then
        if(KILROY) then
          KILROY = .false.
          call LINER  (1,LU)
          call DASHER (LU)
          write (LU,100)
  100     format(' ','Additional Information:')
        end if
      end if
C     !END
      call BYE ('TRAVE')
C
      return
      end
