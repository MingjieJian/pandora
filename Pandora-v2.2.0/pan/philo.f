      subroutine PHILO
     $(EMU,VEX,DLK,WVL0,H1,XN1,XNE,TE,DW,LDL,DDL,CDL,CRD,CVW,CSK,SKE,
     $ CRS,PHI,DMPI,ITAU)
C
C     Rudolf Loeser, 2004 Apr 19
C---- Computes DP and PHI for built-in background contributor lines.
C     !DASH
      save
C     !DASH
      real*8 A, CDL, CON41, CRD, CRS, CSK, CVW, DDL, DLK, DP, DV, DW,
     $       EMU, H1, ONE, PHI, PHII, RS, SK, SKE, T12, T16, T3, T5, TE,
     $       U, VEX, VW, WD, WVL, WVL0, XN1, XNE, ZERO
      integer I, ITAU, LDL, LUEO
      logical DMPI
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  RIGEL, DVOIGT, DIVIDE, LINER, HI, BYE
      intrinsic abs
C
C               DDL(LDL), CDL(LDL), CRD(LDL), CVW(LDL), CSK(LDL),
      dimension DDL(*),   CDL(*),   CRD(*),   CVW(*),   CSK(*),
C
C               CRS(LDL)
     $          CRS(*)
C
      data T16,T12,T5,T3 /1.D16, 1.D12, 5.D3, 0.3D0/
C
      call HI ('PHILO')
C     !BEG
      call RIGEL    (41, CON41)
      DV = (WVL0*CON41)*(EMU*VEX)
C
      if(DMPI) then
        call LINER  (1, LUEO)
        write (LUEO,100) DLK,ITAU,WVL0,EMU,VEX,DV
  100   format(' ','Dump of PHI for DL(k) =',1PE16.9,T50,
     $             'at depth #',I5/
     $         ' ','LM0 =',1PE16.9,', mu =',0PF8.5,', VX =',1PE12.4,
     $             ', DV =',E13.5)
      end if
C     !EJECT
      PHI = ZERO
C
      do 102 I = 1,LDL
C
        SK = CSK(I)*((XNE/T12)**SKE)
        VW = CVW(I)*(H1/T16)*((TE/T5)**T3)
        RS = CRS(I)*(XN1/T16)
        DP = CRD(I)+VW+SK+RS
C
        WVL = abs(DLK-DDL(I)+DV)
        call DIVIDE (ONE, DW, WD)
        U = WD*WVL
        A = WD*DP
        call DVOIGT (U, A, PHII)
        PHI = PHI+CDL(I)*PHII
C
        if(DMPI) then
          write (LUEO,101) I,DDL(I),WVL,U,A,CRD(I),CVW(I),CSK(I),
     $                     CRS(I),DP,CDL(I),PHII,PHI
  101     format(' ','i =',I2,', DDL(i) =',F6.3,', WVL =',1PE16.9,
     $               ', u =',E16.8,', a =',E16.8/
     $           ' ',7X,'CRD(i) =',E12.4,', CVW(i) =',E12.4,
     $               ', CSK(i) =',E12.4,', CRS(i) =',E12.4,
     $               '; DP =',E14.6/
     $           ' ',7X,'CDL(i) =',0PF6.3,', Phi(i) =',1PE16.9,
     $               ';   PHI =',E16.9)
        end if
C
  102 continue
C     !END
      call BYE ('PHILO')
C
      return
      end
