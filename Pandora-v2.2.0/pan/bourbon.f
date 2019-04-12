      subroutine BOURBON
     $(TI,TJ,KASE)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Selects equations for weight matrix calculations.
C     (TI is "TAU", TJ is "T".)
C     (This is version 2 of BOURBON.)
C     !DASH
      save
C     !DASH
      real*8 HNDRD, HNDRDTH, TI, TJ
      integer KASE
C     !DASH
      external HI, BYE
C
      data HNDRDTH,HNDRD /1.D-2, 1.D2/
C
      call HI ('BOURBON')
C     !BEG
      if(TI.le.TJ) then
        if(TJ.ge.HNDRDTH) then
          KASE = 1
        else
          KASE = 2
        end if
      else
        if(TJ.ge.HNDRDTH) then
          KASE = 3
        else
          if(TI.ge.HNDRD*TJ) then
            KASE = 4
          else
            if(TI.le.HNDRDTH) then
              KASE = 5
            else
              KASE = 3
            end if
          end if
        end if
      end if
C     !END
      call BYE ('BOURBON')
C
      return
      end
