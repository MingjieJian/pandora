      subroutine BONBON
     $(NO,IQSLP,KHED)
C
C     Rudolf Loeser, 2004 Aug 27
C---- Explanation of SLF printout.
C     (This is version 2 of BONBON.)
C     !DASH
      save
C     !DASH
      integer IQSLP, KHED, NO, jummy
C     !DASH
      external ERNEST, LINER, HI, BYE
C
      call HI ('BONBON')
C     !BEG
      if(NO.gt.0) then
        if(KHED.eq.0) then
          KHED = 1
          call ERNEST (NO, 3, jummy)
        end if
C
        call LINER    (2, NO)
        if(IQSLP.gt.0) then
          write (NO,100) 'omit', 'off'
  100     format(' ','To ',A,' printout of frequency-dependent ',
     $               'line source function SLF(MS,NS), ',
     $               'turn option SLFPRNT ',A,'.')
        else
          write (NO,100) 'get', 'on'
        end if
        write (NO,101)
  101   format(' ','The summary graph of all SLFs always appears; ',
     $             'the subsequent detail graphs depend on option ',
     $             'SLFGRAF.')
      end if
C     !END
      call BYE ('BONBON')
C
      return
      end
