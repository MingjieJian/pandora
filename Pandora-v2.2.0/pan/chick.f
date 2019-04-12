      subroutine CHICK
     $(XQ,V,F)
C
C     Rudolf Loeser, 1984 Jul 06
C---- Computes integrand, for FIDDLE.
C     !DASH
      save
C     !DASH
      real*8 ART, EX, F, ONE, THREE, TWO, V, XQ, XQ2, XQM, XQP, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C
C---- TAIMEN      as of 1988 Feb 16
      real*8      TAIVS,TAIC1,TAIC2,TAIC3,TAIC4
      common      /TAIMEN/ TAIVS,TAIC1,TAIC2,TAIC3,TAIC4
C     Intermediates for subroutine CHICK.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
      call HI ('CHICK')
C     !BEG
      if(V.ne.TAIVS) then
C----   Initialize V-dependent quantities
C
        if(V.le.ZERO) then
          write (MSSLIN(1),100) V
  100     format('V =',1PE12.4,', which is not greater then 0.')
          call HALT ('CHICK',1)
        end if
C
        TAIVS = V
        TAIC1 = TAIVS**2-ONE
        TAIC2 = TWO*TAIVS
        TAIC3 = TWO/TAIVS
        TAIC4 = (TAIVS**2+ONE)/THREE
      end if
C
      if(XQ.le.ZERO) then
        write (MSSLIN(1),101) XQ
  101   format('XQ =',1PE12.4,', which is not greater than 0.')
        call HALT   ('CHICK',1)
      end if
C
      XQP = ((XQ+V)**2+ONE)**3
      XQM = ((XQ-V)**2+ONE)**3
      XQ2 = XQ**2
      ART = atan2(TAIC2,(XQ2-TAIC1))
      EX  = exp(-TAIC3*ART)
C
      F = ((XQ2+TAIC4)*EX)/(XQ*XQP*XQM)
C     !END
      call BYE ('CHICK')
C
      return
      end
