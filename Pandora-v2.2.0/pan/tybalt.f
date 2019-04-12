      subroutine TYBALT
     $(IU,IL,M,ZRAW,DRAW,CRAW)
C
C     Rudolf Loeser, 1992 Apr 07
C---- Prints for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CRAW, DRAW, ZRAW
      integer I, IL, IU, LUEO, M, ML, MU, N1L, N1U
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, AUK, SHIM, HI, BYE
C
C               ZRAW(M), DRAW(M), CRAW(M)
      dimension ZRAW(*), DRAW(*), CRAW(*)
C
      call HI ('TYBALT')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) IU,IL,M
  100 format(' ','Complete raw tables, sorted, for transition (',I2,
     $           '/',I2,'), M =',I6//
     $       ' ',9X,'i',21X,'DWN',21X,'CDL',5X,'   MU   ML  N1U  N1L')
      call LINER  (1, LUEO)
C
      do 102 I = 1,M
        call AUK  (ZRAW(I), MU, ML, N1U, N1L)
C
        write (LUEO,101) I,DRAW(I),CRAW(I),MU,ML,N1U,N1L
  101   format(' ',I10,1P2E24.16,5X,4I5)
C
        call SHIM (I, 5, LUEO)
  102 continue
C     !END
      call BYE ('TYBALT')
C
      return
      end
