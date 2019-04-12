      subroutine BAMBUSA
     $(I,J,IU,IL,WEIGHT,KIJ,NL,WW)
C
C     Rudolf Loeser, 1968 Aug 28
C---- Retrieves a particular value of WEIGHT.
C     !DASH
      save
C     !DASH
      real*8 WEIGHT, WW, ZERO
      integer I, IL, IU, J, KIJ, NL
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external WAITER, HALT, HI, BYE
C
C               WEIGHT(MUL,NT), KIJ(NL,NL)
      dimension WEIGHT(*),      KIJ(NL,*)
C
      call HI ('BAMBUSA')
C     !BEG
      if(I.eq.J) then
        write (MSSLIN(1),100) I,J
  100   format('I =',I12,', J =',I12,'; they must be different.')
        call HALT   ('BAMBUSA',1)
      end if
C
      WW = ZERO
      if(KIJ(IU,IL).eq.1) then
        call WAITER (I, J, IU, IL, 2, WEIGHT, WW)
      end if
C     !END
      call BYE ('BAMBUSA')
C
      return
      end
