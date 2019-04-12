      subroutine CHILOE
     $(I,KUD,J1,J2)
C
C     Rudolf Loeser, 1987 Nov 27
C---- Sets up indices for CO-lines opacity data arrays.
C     !DASH
      save
C     !DASH
      integer I, J1, J2, KUD
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
      call HI ('CHILOE')
C     !BEG
      if((I.lt.1).or.(I.gt.111)) then
        write (MSSLIN(1),100) I
  100   format('I =',I12,'; which is not 1 through 110, inclusive.')
        call HALT ('CHILOE',1)
      end if
C
      if(KUD.eq.1) then
        J1 = I-1
        J2 = I
      else if(KUD.eq.2) then
        J1 = I
        J2 = I-1
C
      else
        write (MSSLIN(1),101) KUD
  101   format('KUD =',I12,'; which is not 1 or 2 (up-down code).')
        call HALT ('CHILOE',1)
      end if
C     !END
      call BYE ('CHILOE')
C
      return
      end
