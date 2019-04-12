      subroutine EDNA
     $(XNU,IU,IL,FAC)
C
C     Rudolf Loeser, 1990 Oct 02
C---- Computes part of the radiative broadening coefficient:
C     FAC = [ (c/Pi) / {(nu(iu)-nu(il))**2} ] * 1.e-22 / 4.0
C     (This is version 2 of EDNA.)
C     !DASH
      save
C     !DASH
      real*8 CON3, CWARTR, DNU, F, FAC, XNU
      integer IL, IU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(14),CWARTR)
C     !DASH
      external RIGEL, HI, BYE
C
C               XNU(NSL)
      dimension XNU(*)
C
      data F /1.D-22/
C
      call HI ('EDNA')
C     !BEG
      DNU = XNU(IU)-XNU(IL)
      call RIGEL (3, CON3)
C
      FAC = CWARTR*(CON3/(DNU**2))*F
C     !END
      call BYE ('EDNA')
C
      return
      end
