      subroutine WGET
     $(IS,CALLER)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Floating point scratch storage management:
C     returns the start index for the next available chunk.
C     !DASH
      save
C     !DASH
      integer IS
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
C     !EJECT
      external HALT, MPRNT, HI, BYE
C
      call HI ('WGET')
C     !BEG
      if(INEXT.lt.LISTK) then
        IS    = ISTCK(INEXT)
        IUKNT = IUKNT+1
      else
        write (MSSLIN(1),100) CALLER,INEXT,LISTK
  100   format(' ','Fatal error in WGET, called from ',A,5X,'INEXT =',
     $             I10,5X,'LISTK =',I10)
        call HALT  ('WGET', 1)
      end if
      if(WRLDHO) then
        call MPRNT ('x', 'WGET', INEXT, IS, CALLER)
      end if
C     !END
      call BYE ('WGET')
C
      return
      end
