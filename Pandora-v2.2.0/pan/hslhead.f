      subroutine HSLHEAD
     $(IOVER,IOMX,ITHSL,IHSLT)
C
C     Rudolf Loeser, 2000 Jul 12
C---- Prints an HSL-iteration heading.
C     !DASH
      save
C     !DASH
      integer IHSLT, IOMX, IOVER, ITHSL, NO
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
      call HI ('HSLHEAD')
C     !BEG
C---- Invariant printout
      LINE = BLANK
      write (LINE,100) ITHSL,IHSLT,IOVER,IOMX
  100 format('HSL-Iteration',I3,' of',I3,', Overall-Iteration',I3,
     $       ' of',I3,'.')
C
      call LINER   (4,NO)
      write (NO,101) LINE
  101 format(' ',A)
C
C---- Write error file marker, if needed
      call ERRHEAD (LINE)
C     !END
      call BYE ('HSLHEAD')
C
      return
      end
