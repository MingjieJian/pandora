      subroutine TEAL
     $(T,PGS,XION,DEE)
C
C     Rudolf Loeser, 1989 Sep 14
C---- Computes the matrix DEE for the diffusion calculations,
C     for a particular depth, from
C     T    : electron temperature,
C     PGS  : gas pressure, and
C     XION : Hydrogen ionization parameter.
C     (This is version 2 of TEAL.)
C     !DASH
      save
C     !DASH
      real*8 CD22, CD33, CD44, DAI, DAT, DEE, DIFAC, DTFAC, F2, F35,
     $       GAM, P, PGS, PMAX, PMIN, POT1, POT3, POT4, RTX, T, TEXP,
     $       XICA, XICB, XICC, XICD, XION, XTP1, XTP3, XTP4, XXI, XXX,
     $       ZERO
C     !COM
C---- BAMBI       as of 1998 Apr 22
      integer     IPDPAR
      real*8      APDPAR
      dimension   APDPAR(10)
      common      /BAMBI1/ IPDPAR
      common      /BAMBI2/ APDPAR
C     Parameters for "original" d coefficients
C     .
      equivalence
     $(APDPAR( 1),TEXP  ),(APDPAR( 2),XICA  ),(APDPAR( 3),XICB  ),
     $(APDPAR( 4),XICC  ),(APDPAR( 5),DIFAC ),(APDPAR( 6),DTFAC ),
     $(APDPAR( 7),XICD  ),(APDPAR( 8),CD22  ),(APDPAR( 9),CD33  ),
     $(APDPAR(10),CD44  )
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  ZERO1, HI, BYE
      intrinsic max
C
C               DEE(4,5)
      dimension DEE(4,*)
C
      data TEXP                /1.76D0/
      data XICA,XICB,XICC,XICD /1.75D0, 4.5D0, 2.D-2, -3.5D5/
      data DIFAC,DTFAC         /9.07D1, 3.66D1/
      data PMAX,PMIN           /1.D10, 5.D-2/
      data CD22,CD33,CD44      /1.D2, 2.D1, 6.D-9/
      data F2,F35              /2.D0, 3.5D0/
C
      call HI ('TEAL')
C     !BEG
      call ZERO1 (DEE,20)
C
      if((PGS.le.PMAX).and.(XION.gt.ZERO)) then
        P = max(PGS,PMIN)
C
        RTX  = sqrt(XION)
C
        POT1 = T**TEXP
        XTP1 = POT1/P
C
        POT4 = T**F35
        XTP4 = POT4/P
C
        POT3 = T**F2
        XTP3 = POT3/P
C
        GAM = XICC/XION
        XXX = XICA*XION+XICB+XICD*GAM*(RTX/T)
        XXI = XXX/(XION+GAM)
        DAI = DIFAC*XTP1
        DAT = DTFAC*XTP1*XXI
C
        DEE(1,1) = DAI
        DEE(1,5) = DAT
        DEE(2,2) = CD22*XTP1
        DEE(3,3) = CD33*XTP3
        DEE(4,4) = CD44*XTP4
      end if
C     !END
      call BYE ('TEAL')
C
      return
      end
