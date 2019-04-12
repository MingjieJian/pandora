      subroutine DANBUR
     $(NLEV,G,FJIN,FJINL,SFJIN)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Prints, for MINCH.
C     !DASH
      save
C     !DASH
      real*8 FJIN, FJINL, G, SFJIN
      integer I, LUEO, NLEV
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
C               G(NLEV), FJIN(NLEV), FJINL(NLEV)
      dimension G(*),    FJIN(*),    FJINL(*)
C
      call HI ('DANBUR')
C     !BEG
      write (LUEO,100)
  100 format(' ',3X,'N',15X,'G',12X,'FJIN',7X,'log(FJIN)')
      call LINER (1, LUEO)
      write (LUEO,101) (I,G(I),FJIN(I),FJINL(I),I=2,NLEV)
  101 format(5(' ',I4,1P3E16.8/))
      call LINER (1, LUEO)
      write (LUEO,102) SFJIN
  102 format(' ',17X,'Sum',1PE16.8)
C     !END
      call BYE ('DANBUR')
C
      return
      end
