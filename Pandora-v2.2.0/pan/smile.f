      subroutine SMILE
     $(JUL,KIND,LOOK,LENI,LENR,CALLER)
C
C     Rudolf Loeser, 1987 Jan 12
C---- Looks up in the index, and
C     checks record length consistency, for PRD arrays.
C     (This is version 3 of SMILE.)
C     !DASH
      save
C     !DASH
      integer JUL, KIND, LENI, LENR, LOOK
      character CALLER*(*)
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external LOOKUI, HALT, HI, BYE
C
      call HI ('SMILE')
C     !BEG
      if(NTRP.le.0) then
        LOOK = 2
      else
        call LOOKUI  (IPRDTR, NTRP, 0, JUL, KIND, LOOK)
      end if
      if(LOOK.eq.1) then
        LENR = ILNPRD(KIND)
C
        if(LENR.ne.LENI) then
          write (MSSLIN(1),100) LENI,LENR
  100     format('PRD calculation: LENI (=',I12,') does not equal ',
     $           'LENR (=',I12,').')
          call HALT  ((CALLER//' via SMILE'), 1)
        end if
C
      end if
C     !END
      call BYE ('SMILE')
C
      return
      end
