      subroutine LYMHEAD
     $(IOVER,IOMX,ITHSL,IHSLT,LITER,LYMIT)
C
C     Rudolf Loeser, 2000 Jul 12
C---- Prints a Lyman-iteration heading.
C     !DASH
      save
C     !DASH
      integer IHSLT, IOMX, IOVER, ITHSL, LITER, LYMIT, NO
      character BLANK*1, LINE*80
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
      external LINER, ERRHEAD, HI, BYE
C
      call HI ('LYMHEAD')
C     !BEG
C---- Invariant printout
      LINE = BLANK
      write (LINE,100) LITER,LYMIT,ITHSL,IHSLT,IOVER,IOMX
  100 format('Lyman-Iteration',I3,' of',I3,', HSL-Iteration',I3,
     $       ' of',I3,', Overall-Iteration',I3,' of',I3,'.')
C
      call LINER   (4,NO)
      write (NO,101) LINE
  101 format(' ',A)
C
C---- Write error file marker, if needed
      call ERRHEAD (LINE)
C     !END
      call BYE ('LYMHEAD')
C
      return
      end
