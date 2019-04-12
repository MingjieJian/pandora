      subroutine LILTI
     $(II,N,NR,YDAMP)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Dumps for CSF calculation.
C     !DASH
      save
C     !DASH
      real*8 YDAMP
      integer II, LUEO, N, NR
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
      call HI ('LILTI')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) N,II,NR,YDAMP
  100 format(' ','N =',I5,', II =',I5,', NR =',I5,'; Y =',F10.4)
C     !END
      call BYE ('LILTI')
C
      return
      end
