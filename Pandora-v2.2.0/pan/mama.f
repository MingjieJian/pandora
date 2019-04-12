      subroutine MAMA
     $(NW,WT,FHZ,XMG,WREF,Z)
C
C     Rudolf Loeser, 1974 May 30
C---- Computes flux magnitude.
C     !DASH
      save
C     !DASH
      real*8 CON30, FAC, FHZ, WR, WREF, WT, XMG, Z, ZERO, ZWR
      integer I, IRET, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, LININT, MOVE1, CONSUB, CONMUL, HI, BYE
C
C               WT(NW), FHZ(NW), XMG(NW), Z(NW)
      dimension WT(*),  FHZ(*),  XMG(*),  Z(*)
C
      data FAC,WR /-2.5D0, 5.556D3/
C
      call HI ('MAMA')
C     !BEG
      call RIGEL      (30, CON30)
      do 100 I = 1,NW
        if(FHZ(I).gt.ZERO) then
          Z(I) = log10(FHZ(I)/CON30)
        else
          Z(I) = ZERO
        end if
  100 continue
C
      call LININT (WT, 1, Z, 1, NW, WR, ZWR, 1, 1, IRET)
C
      call MOVE1  (Z, NW, XMG)
      call CONSUB (ZWR, XMG, NW)
      call CONMUL (FAC, XMG, NW)
 
      if(IRET.eq.1) then
        WREF = WR
      else if(IRET.eq.3) then
        WREF = WT(1)
      else
        WREF = WT(NW)
      end if
C     !END
      call BYE ('MAMA')
C
      return
      end
