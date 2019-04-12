      subroutine PANGLAO
     $(LENGTH,CALLER)
C
C     Rudolf Loeser, 1986 Jul 08
C---- Checks whether LENGTH .gt. 0, and aborts if not.
C     !DASH
      save
C     !DASH
      integer LENGTH
      character CALLER*(*)
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
      call HI ('PANGLAO')
C     !BEG
      if(LENGTH.le.0) then
        write (MSSLIN(1),100) LENGTH
  100   format('LENGTH =',I12,', which is an invalid record length.')
        call HALT ((CALLER//' via PANGLAO'), 1)
      end if
C     !END
      call BYE ('PANGLAO')
C
      return
      end
