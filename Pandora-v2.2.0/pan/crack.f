      subroutine CRACK
     $(JU,JL,MTR,KIJ,SEM,SFT,YLI,YCO,DPC,OLN,PRD,GMA,UIR,FLX,SFP,
     $ FDB,WAV,BOC,PXC,PXP,PXR,DRUSE,PCE,CFUSE)
C
C     Rudolf Loeser, 1978 Sep 14
C---- Massages transition data for printing.
C     (This is version 2 of CRACK.)
C     !DASH
      save
C     !DASH
      real*8 BOC, DPC, FDB, FLX, GMA, OLN, ONE, PCE, PRD, PXC, PXP, PXR,
     $       SEM, SFP, SFT, TEN, TWO, UIR, WAV, YCO, YLI, ZERO
      integer I, JL, JU, KIJ, MTR
      logical CFUSE, DRUSE, USE
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
      equivalence (DLIT(11),TEN   )
C     !DASH
      external PECCARY, HI, BYE
C
C               SEM(MUL), SFT(MUL), YLI(MUL), YCO(MUL), DPC(MUL),
      dimension SEM(*),   SFT(*),   YLI(*),   YCO(*),   DPC(*),
C
C               PRD(MUL), GMA(MUL), FLX(MUL), JU(MUL) , JL(MUL) ,
     $          PRD(*),   GMA(*),   FLX(*),   JU(*) ,   JL(*) ,
C
C               SFP(MUL), KIJ(MUL), WAV(MUL), BOC(MUL), OLN(MUL),
     $          SFP(*),   KIJ(*),   WAV(*),   BOC(*),   OLN(*),
C
C               FDB(MUL), UIR(MUL), PXC(MUL), PXP(MUL), PXR(MUL),
     $          FDB(*),   UIR(*),   PXC(*),   PXP(*),   PXR(*),
C
C               PCE(MUL)
     $          PCE(*)
C     !EJECT
C
      call HI ('CRACK')
C     !BEG
      do 100 I = 1,MTR
        WAV(I) = WAV(I)/TEN
C
        call PECCARY (JU(I), JL(I), KIJ, USE)
        if(USE) then
          if(SFT(I).gt.ONE) then
            SEM(I) = -ONE
          end if
          if(PRD(I).eq.ZERO) then
            GMA(I) = ZERO
            PXC(I) = ZERO
            PXP(I) = ZERO
            PXR(I) = ZERO
          else
            YLI(I) = TWO
            if((PXC(I).le.ZERO).or.(.not.DRUSE)) then
              PXC(I) = ZERO
              PXP(I) = ZERO
              PXR(I) = ZERO
            end if
          end if
          if(.not.CFUSE) then
            PCE(I) = ZERO
          end if
C
          SEM(I) = SEM(I)+TWO
          YLI(I) = YLI(I)+TEN
          YCO(I) = YCO(I)+TEN
          SFT(I) = SFT(I)+ONE
          DPC(I) = DPC(I)+ONE
          OLN(I) = OLN(I)+ONE
          UIR(I) = UIR(I)+ONE
          FLX(I) = FLX(I)+ONE
          SFP(I) = SFP(I)+ONE
          FDB(I) = FDB(I)+ONE
          PRD(I) = PRD(I)+ONE
          BOC(I) = BOC(I)+ONE
        end if
C
  100 continue
C     !END
      call BYE ('CRACK')
C
      return
      end
