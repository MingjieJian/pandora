      subroutine KULIN
     $(MM,NP,WM1,WM2,WN,WP2,WP1)
C
C     Rudolf Loeser, 1983 Mar 17
C---- Provides dump printout for STORK.
C     (This is version 2 of KULIN.)
C     !DASH
      save
C     !DASH
      real*8 WM1, WM2, WN, WP1, WP2
      integer LUEO, MM, NP, NPM, NPP
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
C               WN(N,N), WM1(NSHL,NSHL), WM2(NSHL,NSHL), WP2(NSHL,NSHL),
      dimension WN(*),   WM1(*),         WM2(*),         WP2(*),
C
C               WP1(NSHL,NSHL)
     $          WP1(*)
C
      call HI ('KULIN')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) MM
  100 format(' ','Shell #',I3)
C
      NPP = NP+1
      NPM = NP-1
C
      call ARROUT (LUEO, WM1, NPP, NPP, 'First Reduced Matrix'   )
      call ARROUT (LUEO, WM2, NP , NP , 'Second Reduced Matrix'  )
      call ARROUT (LUEO, WN , NP , NP , 'Final Average Matrix'   )
      call ARROUT (LUEO, WP2, NP , NP , 'Second Augmented Matrix')
      call ARROUT (LUEO, WP1, NPM, NPM, 'First Augmented Matrix' )
C     !END
      call BYE ('KULIN')
C
      return
      end
