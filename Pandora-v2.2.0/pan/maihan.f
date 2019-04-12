      subroutine MAIHAN
     $(LU,IMAGE,Z1Z,Z2Z,Z3Z,LAB)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Prints plot for KAILUR.
C     !DASH
      save
C     !DASH
      integer LU
      logical Z1Z, Z2Z, Z3Z
      character BLANK*1, IMAGE*(*), LAB*(*), LZ1*4, LZ2*4, LZ3*4
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('MAIHAN')
C     !BEG
      LZ1         = 'Z1, '
      LZ2         = 'Z2, '
      LZ3         = 'Z3, '
      if(Z1Z) LZ1 = BLANK
      if(Z2Z) LZ2 = BLANK
      if(Z3Z) LZ3 = BLANK
C
      call ABJECT (LU)
C
      write (LU,100) LZ1,LZ2,LZ3,LAB
  100 format(' ','Plot 2: logarithmic derivatives ',
     $           'ZT, ZI, ',A,A,A,'vs. ',A,'.')
      call LINER  (1,LU)
C
      call KPRINT (IMAGE,LU)
C     !END
      call BYE ('MAIHAN')
C
      return
      end
