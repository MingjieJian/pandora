      subroutine DUNCAN
     $(KOLEV,TITLE,M)
C
C     Rudolf Loeser, 1993 Jun 14
C---- Sets up M for LYMAN Epsilons calculation.
C     !DASH
      save
C     !DASH
      integer KOLEV, M
      character TITLE*(*)
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
      call HI ('DUNCAN')
C     !BEG
      M = KOLEV
C
      if(M.ne.1) then
        write (MSSLIN(1),100) KOLEV,TITLE
  100   format('KOLEV =',I12,'. When using ',A,' for Lyman epsilons, ',
     $         'KOLEV must = 1.')
        call HALT ('DUNCAN',1)
      end if
C     !END
      call BYE ('DUNCAN')
C
      return
      end
