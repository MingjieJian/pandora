      subroutine GLINI
     $(JUL,LEN,XKPC,BC,SIG,XJNU,RXI,XRD,ZRD,YRD,GOOD)
C
C     Rudolf Loeser, 1986 Dec 17
C---- Saves arrays, for FOG.
C     (This is version 3 of GLINI.)
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
      external SMILE, BOHOL, CEBU, HI, BYE
C
C               XRD(LEN), XKPC(LEN), XJNU(LEN), RXI(LEN), SIG(LEN),
      dimension XRD(*),   XKPC(*),   XJNU(*),   RXI(*),   SIG(*),
C
C               BC(LEN), YRD(LEN), ZRD(LEN)
     $          BC(*),   YRD(*),   ZRD(*)
C
      call HI ('GLINI')
C     !BEG
      GOOD = .true.
      call SMILE    (JUL, KIND, LOOK, LEN, LNG, 'GLINI')
C
      if(LOOK.eq.1) then
C----   Rewrite existing records
        call BOHOL  (XKPC, LNG, IKPADR(KIND))
        call BOHOL  (BC,   LNG, IBCADR(KIND))
        call BOHOL  (SIG,  LNG, ISGADR(KIND))
        call BOHOL  (XJNU, LNG, IJNADR(KIND))
        call BOHOL  (RXI,  LNG, IRXADR(KIND))
        call BOHOL  (XRD,  LNG, IXDADR(KIND))
        call BOHOL  (ZRD,  LNG, IZDADR(KIND))
        call BOHOL  (YRD,  LNG, IYDADR(KIND))
      else
C
        if(NTRP.lt.MXTRP) then
C----     Write initial versions of records
          NTRP = NTRP+1
          IPRDTR(NTRP) = JUL
          ILNPRD(NTRP) = LEN
          call CEBU (XKPC, LEN, IKPADR(NTRP))
          call CEBU (BC,   LEN, IBCADR(NTRP))
          call CEBU (SIG,  LEN, ISGADR(NTRP))
          call CEBU (XJNU, LEN, IJNADR(NTRP))
          call CEBU (RXI,  LEN, IRXADR(NTRP))
          call CEBU (XRD,  LEN, IXDADR(NTRP))
          call CEBU (ZRD,  LEN, IZDADR(NTRP))
          call CEBU (YRD,  LEN, IYDADR(NTRP))
        else
C----     Error: not enough index space allocated
          GOOD = .false.
        end if
C
      end if
C     !END
      call BYE ('GLINI')
C
      return
      end
