      subroutine IOTA
     $(W,N,TITLE,LABEL)
C
C     Rudolf Loeser, 1989 Jul 21
C---- Dump printout for weight matrices.
C     (This is version 2 of IOTA.)
C     !DASH
      save
C     !DASH
      real*8 W
      integer LUEO, N
      character LABEL*(*), TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, ARROUT, HI, BYE
C
C               W(N,N)
      dimension W(*)
C
      call HI ('IOTA')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) TITLE
  100 format(' ',A)
C
      call ARROUT (LUEO, W, N, N, LABEL)
C     !END
      call BYE ('IOTA')
C
      return
      end
