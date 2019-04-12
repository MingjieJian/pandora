      subroutine LEI
     $(ISWTCH,NL,TITLE,NO)
C
C     Rudolf Loeser, 1975 Jan 21
C---- Prints, for RATES.
C     (This is version 2 of LEI.)
C     !DASH
      save
C     !DASH
      integer ISWTCH, J, NL, NO
      character TITLE*(*)
C     !DASH
      external LINER, HI, BYE
C
C               ISWTCH(NL)
      dimension ISWTCH(*)
C
      call HI ('LEI')
C     !BEG
      do 101 J = 1,NL
C
        if(ISWTCH(J).le.0) then
          call LINER (1,NO)
          write (NO,100) TITLE,J
  100     format(' ',20X,'-----',5X,A2,I2,' was not recomputed here.')
        end if
C
  101 continue
C     !END
      call BYE ('LEI')
C
      return
      end
