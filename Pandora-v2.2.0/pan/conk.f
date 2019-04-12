      subroutine CONK
     $(ABDC,ABDO,TE,TCO,HND,ECO,ECH,EOH,CND,OND,CON,CHN,OHN)
C
C     Rudolf Loeser, 2006 Dec 28
C---- Calculates atomic and molecular number densities.
C     (This is version 4 of CONK.)
C     !DASH
      save
C     !DASH
      real*8 A, ABDC, ABDO, B, C, CHN, CND, CON, ECH, ECO, EOH, HND,
     $       OHN, OND, ONE, PCH, POH, TCO, TE, THI, TLO, ZERO
      logical TCBAD, TEBAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FRUIT, GRAIN, HI, BYE
C
      data TLO,THI /0.D0, 9.D3/
C
      call HI ('CONK')
C     !BEG
      TEBAD = (TE .le.TLO).or.(TE .gt.THI)
      TCBAD = (TCO.le.TLO).or.(TCO.gt.THI)
      if(TEBAD.or.TCBAD) then
C
        CON = ZERO
        CHN = ZERO
        OHN = ZERO
C
        ECO = ZERO
        ECH = ZERO
        EOH = ZERO
        OND = ZERO
        CND = ZERO
C
      else
C     !EJECT
C----   Get equilibrium funtions
        call FRUIT (1, TE,  EOH)
        call FRUIT (2, TE,  ECH)
        call FRUIT (3, TCO, ECO)
C----   Intermediates
        POH = ONE+HND*EOH
        PCH = ONE+HND*ECH
C----   For atomic Oxygen
        A = POH*ECO
        B = POH*PCH+(ABDC-ABDO)*HND*ECO
        C = PCH*ABDO*HND
        call GRAIN (A, B, C, OND)
C----   For atomic Carbon
        A = PCH*ECO
        B = PCH*POH+(ABDO-ABDC)*HND*ECO
        C = POH*ABDC*HND
        call GRAIN (A, B, C, CND)
C
C----   Molecular number densities
        OHN = OND*HND*EOH
        CHN = CND*HND*ECH
        CON = CND*OND*ECO
C
      end if
C     !END
      call BYE ('CONK')
C
      return
      end
