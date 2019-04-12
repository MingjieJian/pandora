      subroutine DEAR
     $(XDR,DDR,NDR,XC,XP,DRLIM,DW,DP,TAU,XXC,X,DR)
C
C     Rudolf Loeser, 2002 Sep 17
C---- Computes DR, the absorption fraction of PRD lines.
C     !DASH
      save
C     !DASH
      real*8 DDR, DP, DR, DRLIM, DW, TAU, X, XC, XDR, XP, XXC, ZERO
      integer NDR, jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external BRINK, DRUNK, LININT, HI, BYE
C
C               XDR(NDR), DDR(NDR)
      dimension XDR(*),   DDR(*)
C
      call HI ('DEAR')
C     !BEG
      if(XC.gt.ZERO) then
        call BRINK  (X, XC, XP, DRLIM, DR)
C
      else if(XC.eq.ZERO) then
        call DRUNK  (DW, DP, TAU, XXC)
        call BRINK  (X, XXC, XP, DRLIM, DR)
C
      else
        call LININT (XDR, 1, DDR, 1, NDR, X, DR, 1, 1, jummy)
      end if
C     !END
      call BYE ('DEAR')
C
      return
      end
