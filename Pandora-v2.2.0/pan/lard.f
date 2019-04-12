      subroutine LARD
     $(IDEPTH,TE,V,VXS,CON,H1,XLM,DMPI,STAT)
C
C     Rudolf Loeser, 1992 Sep 17
C---- Computes intermediates for CO-opacity calculation.
C     (This is version 2 of LARD.)
C     !DASH
      save
C     !DASH
      real*8 ANUM, ARG, CB, CF, COMU, CON, CON22, CON25, CON27, CP, DWN,
     $       H1, ONE, PFCO, SRT1, SRT2, TE, V, VPROJ, VXS, WNL, XCOMX,
     $       XFX, XLM, XMCOA, ZERO
      integer IDEPTH
      logical DMPI, STAT
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 52),XCOMX)
      equivalence (RZQ(105),XMCOA)
      equivalence (RZQ(139),COMU )
C
C---- MOLONGA     as of 2003 Dec 02
      integer     I,JUD,L,KOD,M
      real*8      T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,ET,SUM
      logical     DPL,DMP,STT
C
      dimension   C1(2),C3(2),C4(2),WLP(2),WLM(2)
      common      /MOLONG1/ T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,
     $                      ET,SUM
      common      /MOLONG2/ I,JUD,L,KOD,M
      common      /MOLONG3/ DPL,DMP,STT
C     Intermediates for CO-opacity calculation.
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
      external CHOTA, BAGGAGE, WANDA, RIGEL, FELLA, XELLA, HI, BYE
C
      data CB, CF, CP /1.D5, 1.67D-24, 3.D-1/
C     !EJECT
C
      call HI ('LARD')
C     !BEG
      call CHOTA   (TE, V, 1, SRT1)
      call CHOTA   (TE, V, 2, SRT2)
      call XELLA   (TE, XFX)
      call BAGGAGE (TE, ARG, PFCO)
      call WANDA   (XLM, WNL)
      call RIGEL   (22, CON22)
      call RIGEL   (25, CON25)
      call RIGEL   (27, CON27)
      C2     = ONE/WNL
      T      = TE
      ET     = CON22/T
      H      = CON25*(ONE/PFCO)*(CON/CB)
      VPROJ  = COMU*VXS
      SHFT   = VPROJ/CON27
      ANUM   = (CF*(XLM*XFX)*H1)*XMCOA
C
      C1(1)  = CON27/SRT1
      C3(1)  = ANUM/SRT1
      C4(1)  = ONE/SRT1
      DWN    = WNL*(XCOMX*SRT1+VPROJ)/CON27
      WLM(1) = WNL-DWN
      WLP(1) = WNL+DWN
C
      C1(2)  = CON27/SRT2
      C3(2)  = ANUM/SRT2
      C4(2)  = ONE/SRT2
      DWN    = WNL*(XCOMX*SRT2+VPROJ)/CON27
      WLM(2) = WNL-DWN
      WLP(2) = WNL+DWN
C
      DMP = DMPI
      STT = STAT
      if(DMP) then
        call FELLA (XLM, V, VXS, H1, CON, COMU, SRT1, SRT2, XCOMX,
     $              XMCOA, ARG, PFCO, IDEPTH, WNL, XFX)
      end if
C     !END
      call BYE ('LARD')
C
      return
      end
