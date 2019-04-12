      subroutine CORUND
     $(N,XLM,TE,H1,XLMXX,XLMDR,LLY,WLIN,AN1,AN1S,STKFN,CLN,X,IFALL,XLR,
     $ XLTR,EMM,RES,CALL,FALL,FACT,Z,DR,TERM,RAYS,OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes background opacity contributions from the wings of the
C     Hydrogen Lyman N/1 line.
C     (This is version 2 of CORUND.)
C     !DASH
      save
C     !DASH
      real*8 AN1, AN1S, CALL, CLN, CON45, CON46, DR, DRLMN, EMM, ENN2,
     $       ENN3, FACT, FALL, FOUR, FPI, H1, ONE, OPAC, PI, RAYS, RES,
     $       SCAT, STKFN, TE, TERM, WLIN, X, XLM, XLMDR, XLMXX, XLR,
     $       XLTR, Z, ZERO
      integer IFALL, LLY, N
      logical KILROY
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 5),FOUR  )
C     !DASH
      external RIGEL, CUNENE, CUANDO, CUANGO, CUANZA, CUBANGO, CUITO,
     $         HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY)
      dimension XLMXX(*),   XLMDR(*)
C
      data KILROY /.true./
C     !EJECT
C
      call HI ('CORUND')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL (45,CON45)
        call RIGEL (46,CON46)
        FPI = FOUR*PI
      end if
C
      ENN2 = N**2
      ENN3 = N**3
C
      call CUNENE  (N,XLM,WLIN,XLR,XLTR)
      call CUANDO  (N,XLM,TE,XLTR,EMM,RES)
      call CUANGO  (XLM,WLIN,FACT)
      call CUANZA  (N,XLM,Z)
C
      if((N.eq.2).and.(IFALL.eq.1)) then
        call CUITO (XLM,TE,H1,CALL,FALL,TERM)
      else
        CALL = ZERO
        FALL = ZERO
        TERM = CON45*ENN3*((CLN**7)/XLR)*(AN1*AN1)*(H1**2)*RES
      end if
      RAYS = (CON46*ENN2)*AN1*(AN1S+FPI*STKFN)*((CLN**4)/XLR)*H1
C
      DRLMN = AN1S/(AN1+AN1S)
      call CUBANGO (N,XLMXX,XLMDR,LLY,DRLMN,X,DR)
C
      OPAC = (TERM+  RAYS)*FACT*(    DR)
      SCAT = (TERM+Z*RAYS)*FACT*(ONE-DR)
C     !END
      call BYE ('CORUND')
C
      return
      end
