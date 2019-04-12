      subroutine ARRAN
     $(LODE,FLOATS,INTS,N,NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Reads floating or integer arrays, length N, using KIWI and NUDEAL.
C     (Remember that the behavior of NUDEAL is in part controlled
C     by the labelled common blocks declared in KIWI.)
C
C     The input parameter LODE specifies the type of array to be read:
C     LODE=1: floating point (R*8); LODE=2: integer (I*4).
C
C---- The operation of ARRAN is described in: "About PANDORA".
C     !DASH
      save
C     !DASH
      real*8 FLOATS, FVAL, ONE
      integer INTS, IVAL, KERR, LODE, LUEO, MODE, N, jummy
      character ALF*8, GO*8, MESS*5, NAME*8
C     !COM
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external COCK, BUTE, CHLOE, LINER, ABORT, HALT, MESHED, CARMEN,
     $         KIWI, HI, BYE
C
C               FLOATS(N), INTS(N)
      dimension FLOATS(*), INTS(*)
C
      dimension MESS(2)
C
      data GO   /')'/
      data MESS /'FLTPT', 'INTGR'/
C     !EJECT
C
      call HI ('ARRAN')
C     !BEG
      if((LODE.lt.1).or.(LODE.gt.2)) then
        write (MSSLIN(1),100) LODE
  100   format('LODE =',I12,', which is neither 1 nor 2.')
        call HALT   ('ARRAN', 1)
      end if
C
C---- Initialize
      KERR = 0
      IN = 1
      FM = ONE
      IM = 1
      K  = 1
  101 continue
C----   Obtain next input field
        call KIWI   (MODE, FVAL, IVAL, ALF, jummy)
C----   Branch on mode of field
C
C             null      alpha     int       toobig    fltpt
        goto (207,      102,      103,      208,      103       ), MODE
C
  102   continue
C----     Alpha field must be control field - identify it,
C         and jump to appropriate processing section according to type
          KIND = 0
C
          if((ALF.eq.'M').or.(ALF.eq.'m')) KIND = 1
          if((ALF.eq.'F').or.(ALF.eq.'f')) KIND = 2
          if(KIND.gt.0) goto 103
C
          if((ALF.eq.'R').or.(ALF.eq.'r')) KIND = 1
          if((ALF.eq.'I').or.(ALF.eq.'i')) KIND = 2
          if((ALF.eq.'S').or.(ALF.eq.'s')) KIND = 3
C
          if(KIND.gt.0) then
            goto 104
          end if
C
          if(ALF.eq.GO) then
            goto 199
          else
            goto 202
          end if
C
  103   continue
C----     Process array values
          call COCK (MODE, LODE, FVAL, IVAL, FLOATS, INTS, N, NAME)
          goto 101
C
  104   continue
C----     Process array controls, and comments
          call BUTE (NAME)
          goto 101
C     !EJECT
C---- Process errors
  208 KERR = KERR+1
  207 KERR = KERR+1
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED   ('ARRAN', 1)
      write (LUEO,200) MESS(LODE),NAME,'R','r','I','i','S','s',
     $                 'M','m','F','f',GO
  200 format(' ','Error reading ',A5,' array: ',A8//
     $       ' ','List of valid control fields:'//(' ',4X,10A10))
      call CHLOE    (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('ARRAN')
C
      return
      end
