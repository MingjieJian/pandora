      subroutine AMBA
     $(XLM,SIG)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Computes the He Rayleigh scattering cross-section.
C     (See also SIKA.)
C     !DASH
      save
C     !DASH
      real*8 A, ONE, R2, R3, R4, SIG, TL, TS, XLM, ZERO
      integer IRET
C     !DASH
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LININT, HALT, HI, BYE
C
      dimension TL(16), TS(16), A(4)
C
      data      TL / 9.200D+02, 1.100D+03, 1.300D+03, 1.500D+03,
     $               1.700D+03, 1.900D+03, 2.753D+03, 3.132D+03,
     $               3.664D+03, 4.047D+03, 4.359D+03, 4.801D+03,
     $               5.462D+03, 7.247D+03, 8.267D+03, 9.227D+03/
      data      TS / 1.526D-07, 5.841D-08, 2.601D-08, 1.352D-08,
     $               7.779D-09, 4.812D-09, 1.016D-09, 5.977D-10,
     $               3.150D-10, 2.103D-10, 1.556D-10, 1.053D-10,
     $               6.260D-11, 2.010D-11, 1.180D-11, 7.600D-12/
      data      A  / 2.661D-06, 3.790D+02, 6.690D+02, 6.410D+02/
C     !EJECT
C
      call HI ('AMBA')
C     !BEG
      call LININT (TL, 1, TS, 1, 16, XLM, SIG, 1, 1, IRET)
C
      if(IRET.eq.3) then
C----   XLM is less than TL(1)
        SIG = ZERO
C
      else if(IRET.eq.4) then
C----   XLM is greater than TL(16)
        R2  = A(2)/XLM
        R3  = A(3)/XLM
        R4  = A(4)/XLM
        SIG = A(1)*(R2**4)*(ONE+(R3**2)+(R4**4))
C
      else if(IRET.ne.1) then
        write (MSSLIN(1),100) IRET
  100   format('IRET =',I12,', which does not make sense.')
        call HALT ('AMBA', 1)
      end if
C     !END
      call BYE ('AMBA')
C
      return
      end
