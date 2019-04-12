      subroutine NARAVO
     $(NO,NL,NSL)
C
C     Rudolf Loeser, 2003 Mar 07
C---- Prints a warning concerning supplementary levels, for ATOM.
C     !DASH
      save
C     !DASH
      integer NL, NO, NSL
C     !DASH
      external LINER, HI, BYE
C
      call HI ('NARAVO')
C     !BEG
      if(NSL.gt.NL) then
        call LINER  (5, NO)
        write (NO,100)
  100   format(' ','WARNING: we do not know whether PANDORA ',
     $             'currently treats supplementary levels ',
     $             'consistently (03/07/03).')
        call LINER  (5, NO)
      end if
C     !END
      call BYE ('NARAVO')
C
      return
      end
