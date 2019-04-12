      subroutine ILCK
     $(MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Integer scratch storage management:
C     locks up a new chunk (extending up to MUX).
C     !DASH
      save
C     !DASH
      integer I, LUEO, MUX
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
C     !DASH
C     !EJECT
      external  LINER, MESHED, ABORT, MPRNT, HI, BYE
      intrinsic max
C
      call HI ('ILCK')
C     !BEG
      if(MUX.le.JLMIT) then
        JNEXT        = JNEXT+1
        JSTCK(JNEXT) = MUX
        JUMAX        = max(JUMAX,MUX)
      else
        call MESHED ('ILCK', 1)
        write (LUEO,100) CALLER,MUX,JLMIT
  100   format(' ','Fatal error in ILCK, called from ',A,5X,'MUX=',I10,
     $             5X,'JLMUT=',I10)
        call LINER  (2, LUEO)
        write (LUEO,101)
  101   format(' ','Stack contents:')
        write (LUEO,102) (I,JSTCK(I),I=1,JNEXT)
  102   format(' ',I2,I10)
        call ABORT
      end if
      if(WRLDHO) then
        call MPRNT  ('i', 'ILCK', JNEXT, MUX, CALLER)
      end if
C     !END
      call BYE ('ILCK')
C
      return
      end
