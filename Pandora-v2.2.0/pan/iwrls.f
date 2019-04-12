      subroutine IWRLS
     $(CALLER)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Integer scratch storage management:
C     releases the chunk that was most recently locked.
C     !DASH
      save
C     !DASH
      character CALLER*(*)
C     !COM
C---- IWORLD      as of 2002 Jun 04
C
      integer     LJSTK
      parameter   (LJSTK = 100)
      integer     JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
      dimension   JSTCK(LJSTK)
      common      /IWORLD/ JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
C     Management of integer working/scratch storage in IX
C     - JSTCK is the allocation stack
C     - JNEXT is the stack index for the next allocation
C     - JLMIT is the length of IX
C     - JUMAX and JUKNT are cumulative usage statistics.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('IWRLS')
C     !BEG
      if(JNEXT.ge.2) then
        JNEXT = JNEXT-1
      else
        write (MSSLIN(1),100) CALLER,JNEXT
  100   format(' ','Called from ',A,'; JNEXT=',I10)
        call HALT ('IWRLS', 1)
      end if
C     !END
      call BYE ('IWRLS')
C
      return
      end
