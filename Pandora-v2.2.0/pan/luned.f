      subroutine LUNED
     $(LL,XILL,DLLL,ALL)
C
C     Rudolf Loeser, 1982 Sep 22
C---- Dumps, for VULCAN.
C     (This is version 2 of LUNED.)
C     !DASH
      save
C     !DASH
      real*8 ALL, DLLL, XILL
      integer LL, LUEO
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
C               DLLL(1)
      dimension DLLL(*)
C
      call HI ('LUNED')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) LL,XILL,DLLL(1),ALL
  100 format(' ','Frequency  XI(',I3,') =',1PE16.8,5X,'DLLL',E16.8,
     $           5X,'ALL',E16.8)
C     !END
      call BYE ('LUNED')
C
      return
      end
