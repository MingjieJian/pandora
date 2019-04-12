      subroutine BALL
     $(N,Z,T,X,Y,CNXP,RR)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Dumps, for CRANE.
C     !DASH
      save
C     !DASH
      real*8 CNXP, RR, T, X, Y, Z
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
C               Z(N), T(N), X(N), Y(N), CNXP(N), RR(N)
      dimension Z(*), T(*), X(*), Y(*), CNXP(*), RR(*)
C
      call HI ('BALL')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) (I,Z(I),T(I),X(I),Y(I),CNXP(I),RR(I),I=1,N)
  100 format(' ',23X,'Z',17X,'TAU',19X,'X',19X,'Y',16X,'CNXP',18X,'RR'//
     $     5(' ',I4,1P6E20.10/))
C     !END
      call BYE ('BALL')
C
      return
      end
