      subroutine WLCK
     $(MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Floating point scratch storage management:
C     locks up a new chunk (extending up to MUX).
C     !DASH
      save
C     !DASH
      integer I, LUEO, MUX
      character CALLER*(*)
C     !COM
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
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
      call HI ('WLCK')
C     !BEG
      if(MUX.le.ILMIT) then
        INEXT        = INEXT+1
        ISTCK(INEXT) = MUX
        IUMAX        = max(IUMAX,MUX)
      else
        call MESHED ('WLCK', 1)
        write (LUEO,100) CALLER,MUX,ILMIT
  100   format(' ','Fatal error in WLCK, called from ',A,5X,'MUX=',I10,
     $             5X,'ILMIT=',I10)
        call LINER  (2, LUEO)
        write (LUEO,101)
  101   format(' ','Stack contents:')
        write (LUEO,102) (I,ISTCK(I),I=1,INEXT)
  102   format(' ',I2,I10)
        call ABORT
      end if
      if(WRLDHO) then
        call MPRNT  ('x', 'WLCK', INEXT, MUX, CALLER)
      end if
C     !END
      call BYE ('WLCK')
C
      return
      end
