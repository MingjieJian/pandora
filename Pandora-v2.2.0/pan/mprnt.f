      subroutine MPRNT
     $(TYPE,WSUB,INEXT,ISTCK,CALLER)
C
C     Rudolf Loeser, 1997 Oct 09
C---- Messages from "world/iworld" management.
C     !DASH
      save
C     !DASH
      integer INEXT, ISTCK, LUWM, N
      character CALLER*(*), LINE*100, TYPE*1, WSUB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(23),LUWM )
C
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
C     !DASH
      external HI, BYE
C
      data LINE /' '/
C
      call HI ('MPRNT')
C     !BEG
      if(WSUB(2:4).eq.'LCK') then
        N = INEXT-1
      else
        N = INEXT
      end if
  100 format(' ',A1,'world: ',A4,', next =',I3,', stack =',I12,A,A)
C
      if(WRLDTY) then
        write (*,   100) TYPE,WSUB,INEXT,ISTCK,LINE(:N),CALLER
      end if
      if(WRLDPR) then
        write (LUWM,100) TYPE,WSUB,INEXT,ISTCK,LINE(:N),CALLER
      end if
C     !END
      call BYE ('MPRNT')
C
      return
      end
