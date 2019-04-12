      subroutine INDXUL
     $(I,J,IN)
C
C     Rudolf Loeser, 1999 Aug 30
C---- Computes the index of the potential transition [I,J].
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
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('INDXUL')
C     !BEG
      if((I.lt.1).or.(I.gt.NL).or.(J.lt.1).or.(J.gt.NL).or.
     $  (I.le.J)) then
        write (MSSLIN(1),100) I,J,NL
  100   format('Indices and limit:  I =',I10,', J =',I10,', NL =',I10)
        call HALT ('INDXUL',1)
      end if
C
      IN = INC(J,I)
C     !END
      call BYE ('INDXUL')
C
      return
      end
