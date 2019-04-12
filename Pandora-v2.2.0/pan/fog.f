      subroutine FOG
     $(KODE,IU,IL,LEN,XKPC,BC,SIG,XJNU,RXI,XRD,ZRD,YRD)
C
C     Rudolf Loeser, 1986 Dec 17
C---- Saves (KODE=1) or restores (KODE=2) PRD-data-arrays.
C     (This is version 2 of FOG.)
C     !DASH
      save
C     !DASH
      real*8 BC, RXI, SIG, XJNU, XKPC, XRD, YRD, ZRD
      integer I, IL, IU, JUL, KODE, LEN, LUEO
      logical GOOD
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      external GLINI, LISLE, MESHED, ABORT, HALT, LINER, HI, BYE
C
C               XRD(LEN), XKPC(LEN), XJNU(LEN), RXI(LEN), SIG(LEN),
      dimension XRD(*),   XKPC(*),   XJNU(*),   RXI(*),   SIG(*),
C
C               BC(LEN), YRD(LEN), ZRD(LEN)
     $          BC(*),   YRD(*),   ZRD(*)
C
      call HI ('FOG')
C     !BEG
      JUL = 100*IU+IL
C
      if(KODE.eq.1) then
C----   Write or rewrite arrays to scratch file
        call GLINI    (JUL, LEN, XKPC, BC, SIG, XJNU, RXI, XRD, ZRD,
     $                 YRD, GOOD)
        if(.not.GOOD) then
          write (MSSLIN(1),100)
  100     format('Too many PRD transitions; see MXTRP in labelled ',
     $           'common PITCH.')
          call HALT   ('FOG', 1)
        end if
C
      else if(KODE.eq.2) then
C----   Read arrays from scratch file
        call LISLE    (JUL, LEN, XKPC, BC, SIG, XJNU, RXI, XRD, ZRD,
     $                 YRD, GOOD)
        if(.not.GOOD) then
          call MESHED ('FOG', 1)
          write (LUEO,101) (IPRDTR(I),I=1,NTRP)
  101     format(' ',6I20)
          call LINER  (1, LUEO)
          write (LUEO,102) IU,IL
  102     format(' ','PRD transition ',I2,'/',I2,' not found in table.')
          call ABORT
        end if
C
      else
        write (MSSLIN(1),103) KODE
  103   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT     ('FOG', 1)
      end if
C     !END
      call BYE ('FOG')
C
      return
      end
