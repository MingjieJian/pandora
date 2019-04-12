      subroutine GLINT
     $(A,N,QNAME)
C
C     Rudolf Loeser, 1978 Oct 26
C---- Reads alphanumeric arrays.
C     (This is version 2 of GLINT.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer I, ISAV, KERR, LUEO, MODE, N, jummy
      character A*8, QNAME*8, RPAREN*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(50),RPAREN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, MICE, KIWI, MESHED, CHLOE, ABORT, CARMEN, HI, BYE
C
C               A(N)
      dimension A(*)
C     !EJECT
C
      call HI ('GLINT')
C     !BEG
      KERR = 0
      call MACE
C
      do 100 I = 1,N
        ISAV = I
        call KIWI (MODE, dummy, jummy, A(I), jummy)
        if(MODE.ne.2) then
          goto 205
        end if
        if(A(I)(1:1).eq.RPAREN) then
          goto 199
        end if
  100 continue
C
      call MICE
      goto 199
C
C---- Error
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('GLINT', 1)
      write (LUEO,200) QNAME
  200 format(' ','Error reading data for ',A10)
      call CHLOE  (LUEO, A(ISAV), KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('GLINT')
C
      return
      end
