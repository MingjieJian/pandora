      subroutine INTRANS
     $(IU,IL,CALLER,INDEX)
C
C     Rudolf Loeser, 1999 Oct 29
C---- Returns the INDXNT-value for transition [IU/IL].
C     !DASH
      save
C     !DASH
      integer IL, INDEX, IU
      logical OK
      character CALLER*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external INDXNT, HALT, HI, BYE
C
      call HI ('INTRANS')
C     !BEG
      call INDXNT (IU,IL,OK,INDEX)
C
      if(.not.OK) then
        write (MSSLIN(1),100) IU,IL
  100   format('IU =',I10,', IL =',I10,' is not an INPAIR-transition.')
        call HALT ((CALLER//' via INTRANS'),1)
      end if
C     !END
      call BYE ('INTRANS')
C
      return
      end
