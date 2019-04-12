      subroutine SPY
     $(NO)
C
C     Rudolf Loeser, 1979 Jun 21
C---- Prints scratch I/O performance data.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external MASBATE, HI, BYE
C
      call HI ('SPY')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ','Scratch I/O processing performance data')
        call MASBATE (NO)
      end if
C     !END
      call BYE ('SPY')
C
      return
      end
