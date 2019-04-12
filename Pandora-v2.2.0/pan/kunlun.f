      subroutine KUNLUN
     $(MM,NP,WM1,WN,WP1)
C
C     Rudolf Loeser, 1983 Mar 17
C---- Provides dump printout for STORK.
C     (This is version 3 of KUNLUN.)
C     !DASH
      save
C     !DASH
      real*8 WM1, WN, WP1
      integer LUEO, MM, NP
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
C               WN(N,N), WM1(NSHL,NSHL), WP1(NSHL,NSHL)
      dimension WN(*),   WM1(*),         WP1(*)
C
      call HI ('KUNLUN')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) MM
  100 format(' ','Shell #',I3)
C
      call ARROUT (LUEO, WM1, NP, NP, 'Reduced Matrix'      )
      call ARROUT (LUEO, WN , NP, NP, 'Final Average Matrix')
      call ARROUT (LUEO, WP1, NP, NP, 'Augmented Matrix'    )
C     !END
      call BYE ('KUNLUN')
C
      return
      end
