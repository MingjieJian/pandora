      subroutine MORTAL
     $(BACKGR,LINCON)
C
C     Rudolf Loeser, 2003 Mar 26
C---- They may not be equal.
C     !DASH
      save
C     !DASH
      logical BACKGR, LINCON, OK
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('MORTAL')
C     !BEG
      if(BACKGR) then
        OK = .not.LINCON
      else
        OK = LINCON
      end if
C
      if(.not.OK) then
        write (MSSLIN(1),100) BACKGR,LINCON
  100   format('BACGR =',L12,', LINCON =',L12,'; this is not good.')
        call HALT ('MORTAL',1)
      end if
C     !END
      call BYE ('MORTAL')
C
      return
      end
