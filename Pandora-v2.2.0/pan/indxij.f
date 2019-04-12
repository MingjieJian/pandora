      subroutine INDXIJ
     $(I,J,IN)
C
C     Rudolf Loeser, 1999 Aug 30
C---- Computes the index of the level-pair [I,J].
C     !DASH
      save
C     !DASH
      integer I, IN, J, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('INDXIJ')
C     !BEG
      if((I.lt.1).or.(I.gt.NL).or.(J.lt.1).or.(J.gt.NL)) then
        write (MSSLIN(1),100) I,J,NL
  100   format('Indices and limit:  I =',I10,', J =',I10,', NL =',I10)
        call HALT ('INDXIJ',1)
      end if
C
      IN = I+(J-1)*NL
C     !END
      call BYE ('INDXIJ')
C
      return
      end
