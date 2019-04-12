      subroutine WITCH
     $(QNAME,INTEGER)
C
C     Rudolf Loeser, 1072 Feb 08
C---- Reads a single integer.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer INTEGER, KERR, LUEO, MODE, jummy
      character QNAME*8, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, MICE, KIWI, MESHED, CHLOE, ABORT, CARMEN, HI, BYE
C
      call HI ('WITCH')
C     !BEG
      KERR = 0
      call MACE
      call KIWI  (MODE, dummy, INTEGER, qummy, jummy)
      if(MODE.ne.3) goto 203
      call MICE
      goto 199
C
C---- Error processing
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('WITCH', 1)
      write (LUEO,200) QNAME
  200 format(' ','Error while reading for ',A10)
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('WITCH')
C
      return
      end
