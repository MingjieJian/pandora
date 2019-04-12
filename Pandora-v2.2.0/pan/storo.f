      subroutine STORO
     $(KILROY,J,BD,GBFL,E,H,HN,HD)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf emission.
C     !DASH
      save
C     !DASH
      real*8 BD, E, GBFL, H, HD, HN
      integer J, LUEO
      logical KILROY
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
      call HI ('STORO')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call LINER (1, LUEO)
        write (LUEO,100)
  100   format(' ',37X,11X,'GBFL',14X,'E',14X,'H',13X,'HN',13X,'HD')
      end if
      write (LUEO,101) J,BD,GBFL,E,H,HN,HD
  101 format(' ',19X,I3,1P6E15.7)
C     !END
      call BYE ('STORO')
C
      return
      end
