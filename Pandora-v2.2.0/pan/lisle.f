      subroutine LISLE
     $(JUL,LEN,XKPC,BC,SIG,XJNU,RXI,XRD,ZRD,YRD,GOOD)
C
C     Rudolf Loeser, 1986 Dec 17
C---- Restores arrays, for FOG.
C     (This is version 2 of LISLE.)
C     !DASH
      save
C     !DASH
      real*8 BC, RXI, SIG, XJNU, XKPC, XRD, YRD, ZRD
      integer JUL, KIND, LEN, LNG, LOOK
      logical GOOD
C     !COM
C---- PITCH       as of 2006 Jun 14
      integer     MXTRP
      parameter   (MXTRP=10)
C     (Remember to recompile all users when changing MXTRP)
      integer     NTRP,IPRDTR,ILNPRD,IBCADR,IKPADR,IJNADR,IRXADR,
     $                 ISGADR,IXDADR,IYDADR,IZDADR
      dimension   IPRDTR(MXTRP),ILNPRD(MXTRP),IBCADR(MXTRP),
     $            IKPADR(MXTRP),IJNADR(MXTRP),IRXADR(MXTRP),
     $            ISGADR(MXTRP),IXDADR(MXTRP),IYDADR(MXTRP),
     $            IZDADR(MXTRP)
C
      common      /PITCH/ NTRP,IPRDTR,ILNPRD,IBCADR,IKPADR,IJNADR,
     $                         IRXADR,ISGADR,IXDADR,IYDADR,IZDADR
C
C     Record addresses of PRD-data-arrays for up to MXTRP transitions.
C     .
C     !DASH
C     !EJECT
      external SMILE, LEYTE, HI, BYE
C
C               XRD(LEN), XKPC(LEN), XJNU(LEN), RXI(LEN), SIG(LEN),
      dimension XRD(*),   XKPC(*),   XJNU(*),   RXI(*),   SIG(*),
C
C               BC(LEN), YRD(LEN), ZRD(LEN)
     $          BC(*),   YRD(*),   ZRD(*)
C
      call HI ('LISLE')
C     !BEG
      call SMILE   (JUL, KIND, LOOK, LEN, LNG, 'LISLE')
      GOOD = LOOK.eq.1
C
      if(GOOD) then
        call LEYTE (XKPC, LNG, IKPADR(KIND))
        call LEYTE (BC,   LNG, IBCADR(KIND))
        call LEYTE (SIG,  LNG, ISGADR(KIND))
        call LEYTE (XJNU, LNG, IJNADR(KIND))
        call LEYTE (RXI,  LNG, IRXADR(KIND))
        call LEYTE (XRD,  LNG, IXDADR(KIND))
        call LEYTE (ZRD,  LNG, IZDADR(KIND))
        call LEYTE (YRD,  LNG, IYDADR(KIND))
      end if
C     !END
      call BYE ('LISLE')
C
      return
      end
