      subroutine YALTA
     $(DUMP,CALLER)
C
C     Rudolf Loeser, 2003 Oct 09
C---- Selected-depth-index dump trailer.
C     (This is version 2 of YALTA.)
C     !DASH
      save
C     !DASH
      logical DUMP
      character CALLER*(*)
C     !DASH
      external MASHED, HI, BYE
C
      call HI ('YALTA')
C     !BEG
      if(DUMP) then
        call MASHED (CALLER)
      end if
C     !END
      call BYE ('YALTA')
C
      return
      end
