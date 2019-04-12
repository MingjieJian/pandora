      subroutine STORU
     $(FS,EL,EN,HR,REM)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf emission.
C     !DASH
      save
C     !DASH
      real*8 EL, EN, FS, HR, REM
      integer LUEO
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
      call HI ('STORU')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) FS,EL,EN,HR,REM
  100 format(' ',37X,13X,'FS',13X,'EL',13X,'EN',13X,'HR',
     $           15X,12X,'REM'/
     $       ' ',37X,1P4E15.7,15X,E15.7)
C     !END
      call BYE ('STORU')
C
      return
      end
