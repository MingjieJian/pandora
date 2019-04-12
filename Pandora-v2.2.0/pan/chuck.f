      subroutine CHUCK
     $(XQ,XN,F)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Computes integrand, for MUDDLE.
C     !DASH
      save
C     !DASH
      real*8 F, ON, ONE, THREE, XDEN, XN, XNUM, XQ, XQ2, ZERO
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C
C---- THUJA       as of 1988 Feb 16
      real*8      THUSN,THUC1,THUC2,THUC3
      common      /THUJA/ THUSN,THUC1,THUC2,THUC3
C     Intermediates for subroutine CHUCK.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, DIVIDE, HI, BYE
C     !EJECT
C
      call HI ('CHUCK')
C     !BEG
      if(XN.ne.THUSN) then
C----   Set up N-dependent quantities
C
        if(XN.le.ONE) then
          write (MSSLIN(1),100) XN
  100     format('N =',1PE12.4,', which is not greater than 1.')
          call HALT ('CHUCK',1)
        end if
C
        THUSN = XN
        ON    = ONE/THUSN
        THUC1 = (ONE-ONE/(THUSN**2))/THREE
        THUC2 = (ONE-ON)**2
        THUC3 = (ONE+ON)**2
      end if
C
      if(XQ.le.ZERO) then
        write (MSSLIN(1),101) XQ
  101   format('Q =',1PE12.4,', which is not greater than 0.')
        call HALT ('CHUCK',1)
      end if
C
      N = THUSN
      XQ2  = XQ**2
      XNUM = (XQ2+THUC1)*(((XQ2+THUC2)/(XQ2+THUC3))**N)
      XDEN = XQ*((XQ2+THUC2)**3)*((XQ2+THUC3)**3)
C
      call DIVIDE (XNUM,XDEN,F)
C     !END
      call BYE ('CHUCK')
C
      return
      end
