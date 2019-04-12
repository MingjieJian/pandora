      subroutine ANDAMAN
     $(LTYPE,NW,NEW)
C
C     Rudolf Loeser, 1997 Dec 18
C---- Counts the number of "Continuum Eclipse" wavelengths.
C     (This is version 2 of ANDAMAN.)
C     !DASH
      save
C     !DASH
      integer I, LTYPE, NEW, NW
      logical USE
C     !DASH
      external FENNEC, HI, BYE
C
C               LTYPE(NW)
      dimension LTYPE(*)
C
      call HI ('ANDAMAN')
C     !BEG
      NEW = 0
      do 100 I = 1,NW
        call FENNEC (LTYPE(I),USE)
        if(USE) then
          NEW = NEW+1
        end if
  100 continue
C     !END
      call BYE ('ANDAMAN')
C
      return
      end
