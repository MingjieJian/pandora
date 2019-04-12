      subroutine NUTIME
     $(X,IX,W,IW,ICE,XLB1,XLB2,XCBL,STZ,MTRANS,KTRN,JPROM,JLFLX,JPR,
     $ LAST,POUT,PRNT)
C
C     Rudolf Loeser, 2005 Mar 23
C---- PRD calculations for Line Source Function.
C     (This is version 2 of NUTIME.)
C     !DASH
      save
C     !DASH
      real*8 STZ, W, X, XCBL, XLB1, XLB2
      integer IADR, ICE, IN, IS, ITYP, IW, IWAV, IWS, IX, IXLYB, IXPBL,
     $        JLFLX, JN, JPR, JPROM, KTRN, MOX, MTRANS, MUX, NW
      logical KILROY, LAST, POUT, PRNT
C     !DASH
      external ASTARTE, DEMETER, ATTIS, JETLAG, LEOPARD, INDIGO, PENNY,
     $         HUGH, YAGHAN, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len), XCBL(Miklen), STZ(N,2)
      dimension XLB1(*),      XLB2(*),      XCBL(*),      STZ(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWAV  ),(IN( 2),IXPBL ),(IN( 3),IXLYB )
C
      dimension JN(2)
      equivalence
     $(JN( 1),ITYP  ),(JN( 2),IADR  )
C     !EJECT
C
      call HI ('NUTIME')
C     !BEG
      if(ICE.ne.0) then
C
C----   Print iteration marker ( ?)
        call YAGHAN     (JPR, PRNT)
        if(LAST) then
C         Update count of PRD-transitions
          MTRANS = MTRANS+1
        end if
C
C       (Get, and allocate, W & IW allotments)
        call LEOPARD    (IN, IS,  MOX, 'NUTIME')
        call INDIGO     (JN, IWS, MUX, 'NUTIME')
C
C----   Set up subset wavelength table for the current transition
        call ASTARTE    (2, KTRN, NW, W(IWAV), IW(ITYP), IW(IADR))
C
        if(JPR.eq.1) then
C----     Background, Part 1: absorption and emission
          call ATTIS    (X, IX, W, IW, W(IWAV), IW(ITYP), IW(IADR),
     $                   NW, XCBL, XLB1, W(IXPBL), W(IXLYB))
C
          if(POUT.and.(.not.LAST)) then
            call JETLAG (1)
          end if
        end if
        if(POUT.and.LAST) then
          call JETLAG   (2)
        end if
C
C----   PRD terms needed for JNU calculation
        call HUGH       (X, IX, W, IW, IW(IADR), XCBL, XLB1, XLB2,
     $                   JPROM)
C
C----   Background, Part 2: add PRD terms, and calculate JNU and CSF
        call DEMETER    (X, IX, W, IW, W(IWAV), IW(ITYP), IW(IADR),
     $                   NW, XCBL, XLB1, XLB2, JLFLX, JPROM)
C
C----   PRD terms, using JNU, needed for LSF calculation
C       (KILROY is the initialization signal for the JNU save file)
        KILROY = LAST.and.(MTRANS.eq.1)
        call PENNY      (X, IX, W, IW, IW(IADR), XCBL, XLB1, XLB2,
     $                   STZ, JPROM, KILROY, LAST)
        KILROY = .false.
C
C       (Give back W & IW allotments)
        call WGIVE      (W,  'NUTIME')
        call IGIVE      (IW, 'NUTIME')
C
      end if
C     !END
      call BYE ('NUTIME')
C
      return
      end
