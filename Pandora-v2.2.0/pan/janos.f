      subroutine JANOS
     $(AWAVE,ITYPE)
C
C     Rudolf Loeser, 1993 Jun 01
C---- Sets up ITYPE, for KILO.
C     !DASH
      save
C     !DASH
      real*8 AWAVE, ZERO
      integer ITYPE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      call HI ('JANOS')
C     !BEG
      if(AWAVE.gt.ZERO) then
C----   Additional wavelength, no eclipse
        ITYPE = 2
      else if(AWAVE.lt.ZERO) then
C----   Additional wavelength, with eclipse
        ITYPE = 3
      else
        write (MSSLIN(1),100) AWAVE
  100   format('WAVES =',1PE12.4,' is not allowed.')
        call HALT ('JANOS', 1)
      end if
C     !END
      call BYE ('JANOS')
C
      return
      end
