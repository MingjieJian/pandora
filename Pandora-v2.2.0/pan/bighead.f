      subroutine BIGHEAD
     $(IOVER,IOMX)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Prints overall iteration heading.
C     !DASH
      save
C     !DASH
      integer IOMX, IOVER, NO
      character BLANK*1, LINE*80, TIT*12
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external PRIAM, ERRHEAD, LINER, HI, BYE
C
      call HI ('BIGHEAD')
C     !BEG
C---- Section header
      write (TIT,100) IOVER
  100 format('ITERATION',I3)
      call PRIAM   (NO, TIT, 12)
C
C---- Invariant printout
      LINE = BLANK
      write (LINE,101) IOVER,IOMX
  101 format('Overall-Iteration',I3,' of',I3,'.')
C
      call LINER   (4, NO)
      write (NO,102) LINE
  102 format(' ',A)
C
C---- Write error file marker, if needed
      call ERRHEAD (LINE)
C     !END
      call BYE ('BIGHEAD')
C
      return
      end
