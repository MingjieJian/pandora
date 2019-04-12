      subroutine BUTE
     $(NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Processes control fields for ARRAN.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, LUEO, MODE, jummy
      character ALF*8, NAME*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C     !DASH
      external KIWI, CHLOE, MESHED, ABORT, CARMEN, HI, BYE
C     !EJECT
C
      call HI ('BUTE')
C     !BEG
      KERR = 0
C
C           R    I    S
      goto (100, 101, 102 ), KIND
C
  100 continue
C----   Process repeat specification
        call KIWI (MODE, dummy, K , ALF, jummy)
        if(MODE.ne.3) goto 203
        goto 199
  101 continue
C----   Process index specification
        call KIWI (MODE, dummy, IN, ALF, jummy)
        if(MODE.ne.3) goto 203
        goto 199
  102 continue
C----   Process skip specification
        IN = IN+K
        K  = 1
        goto 199
C
C---- Process errors
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('BUTE', 1)
      write (LUEO,200) NAME
  200 format(' ','Error reading for ',A8)
      call CHLOE  (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('BUTE')
C
      return
      end
