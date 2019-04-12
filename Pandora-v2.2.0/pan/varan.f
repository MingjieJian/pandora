      subroutine VARAN
     $(NO,IZERO,NAME)
C
C     Rudolf Loeser, 2002 May 06
C---- Prints an input-zero-print-mode message.
C     !DASH
      save
C     !DASH
      integer IZERO, NO
      character NAME*(*)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('VARAN')
C     !BEG
      if(NO.gt.0) then
        if(IZERO.eq.1) then
          write (NO,100) NAME
  100     format(' ',10X,'To omit parameter values = 0, and to omit ',
     $               'lines of all = 0, revert to the default (= 0)'/
     $           ' ',10X,'of input parameter ',A,'.')
        else
          write (NO,101) NAME
  101     format(' ',10X,'Parameter values = 0, and lines of all = 0, ',
     $               'are normally omitted; to show them,'/
     $           ' ',10X,'set input parameter ',A,' = 1.')
        end if
        call LINER (1, NO)
      end if
C     !END
      call BYE ('VARAN')
C
      return
      end
