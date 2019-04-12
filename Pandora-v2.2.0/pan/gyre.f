      subroutine GYRE
     $(IU,IL,R1N,Z,BCTRN,GTN,COPTRN,STRN,DP,DW,N,K)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Prints debug output, for LEAVES.
C     (This is version 2 of GYRE.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, COPTRN, DP, DW, GTN, R1N, STRN, Z
      integer I, IL, IU, K, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ABJECT, LINER, ARROUT, HI, BYE
C
C               GTN(N), DP(N), DW(N), BCTRN(N,KM), COPTRN(N,KM), Z(N),
      dimension GTN(*), DP(*), DW(*), BCTRN(*),    COPTRN(*),    Z(*),
C
C               STRN(N,KM)
     $          STRN(*)
C
      call HI ('GYRE')
C     !BEG
      call ABJECT (LUEO)
      write (LUEO,100) IU,IL,R1N
  100 format(' ','Details of Line (',I2,',',I2,') Intensity ',
     $           'calculation in spherical coordinates.'//
     $       ' ','R1N =',1PE14.4//
     $       ' ',19X,'Z',13X,'GTN',14X,'DP',14X,'DW')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,Z(I),GTN(I),DP(I),DW(I),I=1,N)
  101 format(5(' ',I4,1P4E16.8/))
C
      call ARROUT (LUEO, BCTRN,  N, K, 'BC' )
      call ARROUT (LUEO, COPTRN, N, K, 'COP')
      call ARROUT (LUEO, STRN,   N, K, 'S'  )
C     !END
      call BYE ('GYRE')
C
      return
      end
