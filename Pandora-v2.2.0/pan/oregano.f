      subroutine OREGANO
     $(WEIGHT,QNAME)
C
C     Rudolf Loeser, 1968 Apr 23
C---- Reads the "Weights" for the calculation
C     of the Statistical Equilibrium Equations.
C     !DASH
      save
C     !DASH
      real*8 WEIGHT, X
      integer I, IL, IU, J, KERR, LUEO, MODE, jummy
      character ALFA*8, QNAME*8, RPAREN*1, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(50),RPAREN)
C     !DASH
      external MACE, KIWI, MINT, WAITER, MESHED, CHLOE, ABORT, CARMEN,
     $          HI, BYE
C
C               WEIGHT(MUL,NT)
      dimension WEIGHT(*)
C     !EJECT
C
      call HI ('OREGANO')
C     !BEG
      KERR = 0
      call MINT       (QNAME, IU)
      call MINT       (QNAME, IL)
      call MACE
C---- Read next field, which must be either integer or ")"
  100 continue
        call KIWI     (MODE, X, I, ALFA, jummy)
        if(MODE.eq.2) then
C----     It is alphanumeric - check for ")"
          if(ALFA.ne.RPAREN) goto 205
          goto 199
        else if(MODE.eq.3) then
C----     It is integer, therefore first index. Read next index.
          call MINT   (QNAME, J)
C----     Now read value, ...
          call KIWI   (MODE, X, jummy, qummy, jummy)
          if(MODE.ne.5) goto 204
C-----    ... and store it
          call WAITER (I, J, IU, IL, 1, WEIGHT, X)
          goto 100
        else
          goto 205
        end if
C
C---- Error processing
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED     ('OREGANO', 1)
      write (LUEO,200) QNAME
  200 format(' ','Error reading for ',A10,' List of expected fields:'/
     $       ' ',4X,')',9X,'[integer]')
      call CHLOE      (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('OREGANO')
C
      return
      end
