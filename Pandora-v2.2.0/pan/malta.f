      subroutine MALTA
     $(XLM,DUMP,CALLER)
C
C     Rudolf Loeser, 2003 Oct 09
C---- Selected-depth-index dump header.
C     (This is version 2 of MALTA.)
C     !DASH
      save
C     !DASH
      real*8 XLM
      logical DUMP
      character CALLER*(*)
C     !DASH
      external DACAPO, MESHED, HI, BYE
C
      call HI ('MALTA')
C     !BEG
      if(DUMP) then
        call DACAPO (XLM)
        call MESHED (CALLER, 2)
      end if
C     !END
      call BYE ('MALTA')
C
      return
      end
