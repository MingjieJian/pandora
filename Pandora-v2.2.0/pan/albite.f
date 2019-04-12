      subroutine ALBITE
     $(XN,XQ,F,A,N)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Dumps, for MUDDLE.
C     !DASH
      save
C     !DASH
      real*8 A, F, XN, XQ
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               XQ(N), F(N), A(N)
      dimension XQ(*), F(*), A(*)
C
      call HI ('ALBITE')
C     !BEG
      call LINER (5, LUEO)
      write (LUEO,100) XN
  100 format(' ','Details for N =',F5.1//
     $       ' ',23X,'Q',15X,'F',15X,'A')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,XQ(I),F(I),A(I),I=1,N)
  101 format(5(' ',I8,1P3E16.8/))
      call LINER (5, LUEO)
C     !END
      call BYE ('ALBITE')
C
      return
      end
