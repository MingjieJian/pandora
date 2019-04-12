      subroutine MUSKEG
     $(IU,IL,XNU,TE,XNE,POPK,NO,WW,RM,VION,TIB,CIB)
C
C     Rudolf Loeser, 1990 Oct 04
C---- Sets up TIB and CIB, the ion-broadening terms.
C     !DASH
      save
C     !DASH
      real*8 CIB, FAC, ONE, POPK, RM, TE, TIB, TRM, TWO, VION, WW, XNE,
     $       XNU, ZERO
      integer IL, IU, N, NO
      logical WWZERO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external HOSEAH, EDNA, NAUGHTD, CONMUL, MOVE1, ZERO1, HI, BYE
C
C               RM(NPOPS), TE(N), XNE(N), POPK(N,NPOPS), WW(N), TIB(N),
      dimension RM(*),     TE(*), XNE(*), POPK(*),       WW(*), TIB(*),
C
C               XNU(NSL), VION(NPOPS)
     $          XNU(*),   VION(*)
C
      call HI ('MUSKEG')
C     !BEG
      call HOSEAH   (N, XNU, TE, XNE, POPK, RM, VION, NO, WW)
      call NAUGHTD  (WW, 1, N, WWZERO)
      if(.not.WWZERO) then
C
        CIB = ONE
C
        call EDNA   (XNU, IU, IL, FAC)
        TRM = TWO*FAC
        call MOVE1  (WW, N, TIB)
        call CONMUL (TRM, TIB, N)
C
      else
        CIB = ZERO
        call ZERO1  (TIB, N)
      end if
C     !END
      call BYE ('MUSKEG')
C
      return
      end
