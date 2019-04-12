      subroutine GRIZZLY
     $(NO,IU,IL,LDL,DDL,DWN,CDL,CRD,CVW,CSK,CORE)
C
C     Rudolf Loeser, 1985 Jun 19
C---- Prints Blended Line components.
C     !DASH
      save
C     !DASH
      real*8 CDL, CORE, CRD, CSK, CVW, CWVN, DDL, DWN, WVL, WVN
      integer I, IL, IU, LDL, NO
C     !DASH
      external  WANDA, LINER, HI, BYE
      intrinsic min
C
C               DDL(LDL), CDL(LDL), CRD(LDL), CVW(LDL), CSK(LDL),
      dimension DDL(*),   CDL(*),   CRD(*),   CVW(*),   CSK(*),
C
C               DWN(LDL)
     $          DWN(*)
C
      call HI ('GRIZZLY')
C     !BEG
      call LINER   (1, NO)
      call WANDA   (CORE, CWVN)
      write (NO,100) IU,IL,CORE,CWVN
  100 format(' ',I2,'/',I2,1PE23.14,15X,E23.14)
      call LINER   (1, NO)
C
      do 102 I = 1,LDL
        WVL = CORE+DDL(I)
        call WANDA (WVL, WVN)
        write (NO,101) WVL,DDL(I),WVN,DWN(I),CDL(I),
     $                                       CRD(I),CVW(I),CSK(I)
  101   format(' ',5X,1PE23.14,E15.7,E23.14,E15.7,0PF10.4,1P3E12.4)
  102 continue
C     !END
      call BYE ('GRIZZLY')
C
      return
      end
