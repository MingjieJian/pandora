      subroutine MINNA
     $(DUMP,I,INCD,DMPI)
C
C     Rudolf Loeser, 2003 Oct 07
C---- Determines whether a dump is needed at this depth.
C     !DASH
      save
C     !DASH
      integer I, INCD
      logical DMPI, DUMP
C     !DASH
      external PINNA, HI, BYE
C
      call HI ('MINNA')
C     !BEG
      DMPI = .false.
      if(DUMP) then
        call PINNA (I, INCD, DMPI)
      end if
C     !END
      call BYE ('MINNA')
C
      return
      end
