      subroutine WALL
     $(N,T,R,B,CNDT,BB,CNXP)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Dumps, for SASKIA.
C     !DASH
      save
C     !DASH
      real*8 B, BB, CNDT, CNXP, R, T
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
C               T(N), R(N), B(N), CNDT(N), BB(N), CNXP(N)
      dimension T(*), R(*), B(*), CNDT(*), BB(*), CNXP(*)
C
      call HI ('WALL')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) (I,T(I),R(I),B(I),CNDT(I),BB(I),CNXP(I),I=1,N)
  100 format(' ',23X,'T',19X,'R',19X,'B',16X,'CNDT',
     $             18X,'BB',16X,'CNXP'//
     $     5(' ',I4,1P6E20.10/))
C     !END
      call BYE ('WALL')
C
      return
      end
