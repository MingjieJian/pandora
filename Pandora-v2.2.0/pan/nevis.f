      subroutine NEVIS
     $(ION,TE,LU,CI)
C
C     Rudolf Loeser, 2006 Nov 17
C---- Computes O I CI(1) using data from
C     W.R. Thompson, M.B. Shah, & H.B. Gilbody, 1995,J.Phys.B.,28,1321.
C     (This is version 2 of NEVIS.)
C     !DASH
      save
C     !DASH
      real*8 CAN, CI, CON, FAC, RT, SE, SIGMA, TE, XI, XNU, XNUK, Z,
     $       ZERO
      integer I, ION, LU, NT
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HART, HI, BYE
C
      parameter (NT=30)
      dimension XNU(NT), SIGMA(NT), Z(NT), XNUK(1)
C
      data XNUK /3.2928D0/
      data CAN,CON,STEP /1.4309D15, 47992.D0, 5.0D-2/
C
      data XNU /
     $  0.000D0,  3.409D0,  3.458D0,  3.506D0,  3.554D0,
     $  3.627D0,  3.699D0,  3.796D0,  3.893D0,  4.014D0,
     $  4.304D0,  4.642D0,  5.223D0,  5.610D0,  7.060D0,
     $  7.544D0,  8.584D0,  9.793D0, 11.123D0, 12.090D0,
     $ 13.541D0, 15.233D0, 17.168D0, 19.344D0, 21.762D0,
     $ 23.454D0, 27.081D0, 30.466D0, 35.544D0, 39.896D0/
C
      data SIGMA /
     $  0.00D-17,  0.24D-17,  0.34D-17,  0.47D-17,  0.41D-17,
     $  0.53D-17,  0.69D-17,  0.75D-17,  0.99D-17,  1.10D-17,
     $  1.52D-17,  2.11D-17,  3.61D-17,  4.36D-17,  6.27D-17,
     $  7.12D-17,  7.61D-17, 10.10D-17, 11.80D-17, 12.20D-17,
     $ 12.40D-17, 12.50D-17, 13.00D-17, 13.10D-17, 14.40D-17,
     $ 14.60D-17, 14.50D-17, 14.50D-17, 14.30D-17, 13.70D-17/
C     !EJECT
C
      call HI ('NEVIS')
C     !BEG
      CI = ZERO
C
      if(((ION.ge.1).and.(ION.le.1)).and.(TE.ne.ZERO)) then
        DUMP = LU.gt.0
        if(DUMP) then
          write (LU,100) ION,TE
  100     format(' ','Dump from Oxygen-',I1,' CI1(T =',1PE12.4,')'//
     $           ' ',16X,'nu',11X,'exp',9X,'Sigma',5X,'integrand')
        end if
        XNU(1) = XNUK(ION)
        do 102 I = 1,NT
          SE   = exp(-(CON*(XNU(I)-XNUK(ION)))/TE)
          Z(I) = XNU(I)*(SIGMA(I)*SE)
          if(DUMP) then
            write (LU,101) I,XNU(I),SE,SIGMA(I),Z(I)
  101       format(' ',I4,1P4E14.6)
          end if
  102   continue
        call HART    (NT, XNU, Z, XI)
C
        RT  = sqrt(TE)
        FAC = CAN/(RT**3)
C
        CI = FAC*XI
        if(DUMP) then
          write (LU,103) XI,FAC,CI
  103     format(' ','integral =',1PE14.6,5X,'factor =',E14.6,5X,
     $               'CI(1) =',E14.6)
        end if
      end if
C     !END
      call BYE ('NEVIS')
C
      return
      end
