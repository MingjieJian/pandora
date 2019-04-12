      subroutine FERMENT
     $(INDX,WVL,SIG0,FE)
C
C     Rudolf Loeser, 2004 Nov 02
C---- Returns SIG0 and FE, for the INDX'th element.
C     If the input value of WVL .le. 0, uses E = ETH;
C                               .gt. 0, uses E = 12398/WVL.
C
C     Verner et al., 1996, ApJ, 465, 487.
C
C     !DASH
      save
C     !DASH
      real*8 CNST, E, E0, EANG, ETH, FE, HALF, ONE, P, RT, SIG0, T1, T2,
     $       T3, WVL, X, Y, Y0, Y1, YA, YW, ZERO, dummy
      integer INDX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FERVENT, HI, BYE
C
      data CNST,EANG /5.5D0, 1.2398D4/
C
      call HI ('FERMENT')
C     !BEG
      call FERVENT (INDX, ETH, E0, dummy, SIG0, YA, P, YW, Y0, Y1)
C
      if(WVL.le.ZERO) then
        E = ETH
      else
        E = EANG/WVL
      end if
C
      X  = E/E0-Y0
      Y  = sqrt((X**2)+(Y1**2))
      RT = sqrt(Y/YA)
C
      T1 = (ONE+RT)**(-P)
      T2 = Y**(HALF*P-CNST)
      T3 = (X-ONE)**2+YW**2
C
      FE = T1*T2*T3
C     !END
      call BYE ('FERMENT')
C
      return
      end
