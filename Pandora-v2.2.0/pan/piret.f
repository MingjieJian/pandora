      subroutine PIRET
     $(N,TAU,SN,S,RHO,XJBAR,Z,VSB,FXI,YBRC,DP,DW,ISB1,ISB2,IMG,W)
C
C     Rudolf Loeser, 1986 Jul 24
C---- Sets up values of S, RHO and JBAR,
C     using an escape probability approximation.
C     !DASH
      save
C     !DASH
      real*8 DP, DW, FXI, RHO, S, SN, TAU, VSB, W, XJBAR, YBRC, Z
      integer IDV, IFF, IFO, IG, IGC, IH, IMG, IN, IPD, IRHOSO, IRHOST,
     $        IRV, IS, ISB1, ISB2, MOX, N
C     !DASH
      external IVAR, DRONGO, SKUA, BECARD, MOVE1, ELBOW, DURANKI, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               SN(N), VSB(N), XJBAR(N), RHO(N), TAU(N), DP(N), IMG(N),
      dimension SN(*), VSB(*), XJBAR(*), RHO(*), TAU(*), DP(*), IMG(*),
C
C               FXI(N), YBRC(N), Z(N), S(N), DW(N,LDL)
     $          FXI(*), YBRC(*), Z(*), S(*), DW(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),IRHOST),(IN( 2),IRHOSO),(IN( 3),IDV   ),(IN( 4),IRV   ),
     $(IN( 5),IG    ),(IN( 6),IPD   ),(IN( 7),IH    ),(IN( 8),IFF   ),
     $(IN( 9),IGC   ),(IN(10),IFO   )
C     !EJECT
C
      call HI ('PIRET')
C     !BEG
C     (Get, and allocate, W allotment)
      call IVAR     (IN,IS,MOX,'PIRET')
C
C---- Compute static solution
      call DRONGO   (N,TAU,DP,DW,W(IRHOST))
      if(ISB1.gt.1) then
C----   Compute Sobolev solution
        call SKUA   (N,Z,VSB,FXI,SN,YBRC,W(IRHOST),W(IRHOSO),W(IDV),
     $               W(IRV),W(IH),W(IG),W(IGC),W(IPD),W(IFF),ISB1,ISB2)
C----   Combine solutions
        call BECARD (N,ISB1,ISB2,W(IRHOST),W(IRHOSO),RHO)
      else
C----   Use only static solution
        call MOVE1  (W(IRHOST),N,RHO)
      end if
C---- Compute S and JBAR
      call MOVE1    (SN,N,S)
      call DURANKI  (S,RHO,XJBAR,N)
C---- Edit JBAR
      call ELBOW    (XJBAR,IMG,W(IFO),N,'Esc. probab.')
C
C     (Give back W allotment)
      call WGIVE    (W,'PIRET')
C     !END
      call BYE ('PIRET')
C
      return
      end
