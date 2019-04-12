      subroutine AQUAMA
     $(N,HN,XNUM,XDEN,SHLN)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints details, for CALAMUS.
C     (This is version 2 of AQUAMA.)
C     !DASH
      save
C     !DASH
      real*8 HN, SHLN, XDEN, XNUM
      integer LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C
      call HI ('AQUAMA')
C     !BEG
      write (LUEO,100) N,HN,XNUM,XDEN,SHLN
  100 format(' ','N =',I3,5X,'HN =',1PE16.8,5X,'num =',E16.8,5X,
     $           'den =',E16.8,5X,'SHLN =',E16.8)
C     !END
      call BYE ('AQUAMA')
C
      return
      end
