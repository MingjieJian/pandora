      subroutine IRLS
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
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
C     !DASH
      external HALT, MPRNT, HI, BYE
C
      call HI ('IRLS')
C     !BEG
      if(JNEXT.ge.2) then
        JNEXT = JNEXT-1
      else
        write (MSSLIN(1),100) CALLER,JNEXT
  100   format(' ','Fatal error in IRLS, called from ',A,5X,'JNEXT=',
     $             I10)
        call HALT  ('IRLS', 1)
      end if
      if(WRLDHO) then
        call MPRNT ('i', 'IRLS', JNEXT, JSTCK(JNEXT), CALLER)
      end if
C     !END
      call BYE ('IRLS')
C
      return
      end
