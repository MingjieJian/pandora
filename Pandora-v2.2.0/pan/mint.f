      subroutine MINT
     $(QNAME,INDEX)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Reads a single index.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer INDEX, KERR, LUEO, MODE, jummy
      character QNAME*8, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KIWI, MESHED, CHLOE, ABORT, HI, BYE
C
      call HI ('MINT')
C     !BEG
      KERR = 0
      call KIWI  (MODE, dummy, INDEX, qummy, jummy)
      if(MODE.ne.3) then
        goto 203
      end if
      goto 199
C
C---- Error
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('MINT', 1)
      write (LUEO,200) QNAME
  200 format(' ','Error in MINT, while reading for ',A8)
      call CHLOE (LUEO, QNAME, KERR)
      call ABORT
      INDEX = 1
C
C---- Go home
  199 continue
C     !END
      call BYE ('MINT')
C
      return
      end
