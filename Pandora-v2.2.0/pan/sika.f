      subroutine SIKA
     $(XLM,SIG)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes the H2 Rayleigh scattering cross-section.
C     (See also AMBA.)
C     !DASH
      save
C     !DASH
      real*8 A, SIG, TL, TS, X2, X4, XLM, ZERO
      integer IRET
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
      external LININT, HALT, HI, BYE
C
      dimension TL(22), TS(22), A(3)
C
      data TL / 1.2000D+03, 1.2157D+03, 1.3000D+03, 1.4000D+03,
     $          1.5000D+03, 1.6000D+03, 1.7000D+03, 1.8546D+03,
     $          1.8627D+03, 1.9358D+03, 1.9905D+03, 2.3029D+03,
     $          2.3791D+03, 2.5356D+03, 2.7536D+03, 2.9681D+03,
     $          3.3424D+03, 4.0477D+03, 4.0790D+03, 4.3596D+03,
     $          5.4623D+03, 6.3280D+03/
C
      data TS / 2.3500D-06, 2.3500D-06, 1.2200D-06, 6.8000D-07,
     $          4.2400D-07, 2.8400D-07, 2.0000D-07, 1.2500D-07,
     $          1.2200D-07, 1.0000D-07, 8.7000D-08, 4.2900D-08,
     $          3.6800D-08, 2.7500D-08, 1.8900D-08, 1.3600D-08,
     $          8.1100D-09, 3.6000D-09, 3.4800D-09, 2.6400D-09,
     $          1.0400D-09, 5.6900D-10/
C
      data A  / 8.7790D+05, 1.3230D+12, 2.2450D+18/
C     !EJECT
C
      call HI ('SIKA')
C     !BEG
      call LININT (TL, 1, TS, 1, 22, XLM, SIG, 1, 1, IRET)
C
      if(IRET.eq.3) then
C----   XLM is less than TL(1)
        SIG = ZERO
C
      else if(IRET.eq.4) then
C----   XLM is greater than TL(22)
        X2 = XLM**2
        X4 = X2**2
        SIG = (A(1)+(A(2)+A(3)/X2)/X2)/X4
C
      else if(IRET.ne.1) then
        write (MSSLIN(1),100) IRET
  100   format('IRET =',I12,', which does not make sense.')
        call HALT ('SIKA', 1)
      end if
C     !END
      call BYE ('SIKA')
C
      return
      end
