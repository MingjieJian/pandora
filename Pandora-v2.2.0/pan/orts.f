      subroutine ORTS
     $(KAR,BUF,NAME,MODE)
C
C     Rudolf Loeser, 1992 Oct 21
C---- Prints a line buffer for NAGA.
C     (This is version 2 of ORTS.)
C     !DASH
      save
C     !DASH
      integer KAR, LUEO, MODE
      logical FLUSH
      character BLANK*1, BUF*120, NAME*4
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
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
      call HI ('ORTS')
C     !BEG
      FLUSH = (MODE.eq.1).and.(KAR.gt.0)
      if((KAR.eq.120).or.FLUSH) then
        write (LUEO,100) NAME,BUF
  100   format(' ',A4,3X,A120)
C
        KAR = 0
        BUF = BLANK
      end if
C     !END
      call BYE ('ORTS')
C
      return
      end
