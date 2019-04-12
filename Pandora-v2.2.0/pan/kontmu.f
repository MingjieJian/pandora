      subroutine KONTMU
     $(NO)
C
C     Rudolf Loeser, 2006 Mar 22
C---- Provides a header for AMASIS: repeatedly if needed.
C     (This is version 3 of KONTMU.)
C     !DASH
      save
C     !DASH
      integer NO, NUMSAV
C     !COM
C---- ILION       as of 1987 Aug 20
      integer     NUMSCT
      common      /ILION/ NUMSCT
C     Next available identifier for printout sections.
C     .
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      data NUMSAV /-1/
C
      call HI ('KONTMU')
C     !BEG
      if(NUMSAV.ne.NUMSCT) then
        call PRIAM (NO, 'PROF-PREP', 9)
        NUMSAV = NUMSCT
        call LINER (1, NO)
        write (NO,100)
  100   format(' ','Background (continuum) calculations are done ',
     $             'for the following:')
        call LINER (2, NO)
      end if
C     !END
      call BYE ('KONTMU')
C
      return
      end
