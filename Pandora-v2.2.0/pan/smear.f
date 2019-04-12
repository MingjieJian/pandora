      subroutine SMEAR
     $(IMAGE,NO,N,IC,JMIN,JMAX)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Writes out the completed Check graph.
C     (This is version 2 of SMEAR.)
C     !DASH
      save
C     !DASH
      integer I, IC, JMAX, JMIN, K, LINO, N, NO
      character IMAGE*(*), LINE*117
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external  KGIVE, ABJECT, LINER, HI, BYE
      intrinsic min
C
      call HI ('SMEAR')
C     !BEG
      call ABJECT  (NO)
C
      K = min(JMAX,26)
      write (NO,100) IC,JMIN,ALPHS(1),JMAX,ALPHS(K)
  100 format(' ','Graph of CHECK ',I2,
     $           '  vs. indices of Z-points, for iteration',I4,
     $           '(',A,') through',I4,'(',A,')'//
     $       ' ','The quantity plotted is log10(CHECK-1)'//
     $       ' ',64X,'-.001'/
     $       ' ',8X,'-1.',16X,'-.1',15X,'-.01',15X,'+.001',
     $           15X,'+.01',16X,'+.1',17X,'+1.')
C
      LINO = 0
      do 103 I=1,(N-1)
        LINO = LINO+1
        call KGIVE (IMAGE,LINO,LINE)
        write (NO,101) I,LINE
  101   format(' ',I4,5X,A)
  102   format(' ',9X,A)
        LINO = LINO+1
        call KGIVE (IMAGE,LINO,LINE)
        write (NO,102) LINE
  103 continue
C
      LINO = LINO+1
      call KGIVE   (IMAGE,LINO,LINE)
      write (NO,101) N,LINE
C     !END
      call BYE ('SMEAR')
C
      return
      end
