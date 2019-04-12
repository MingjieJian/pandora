      subroutine STERK
     $(XLM,TIT)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Header for H-bf dumps.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer LUEO
      character TIT*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DASHER, LINER, HI, BYE
C
      call HI ('STERK')
C     !BEG
      call LINER  (1, LUEO)
      call DASHER (LUEO)
      call LINER  (1, LUEO)
      write (LUEO,100) TIT,XLM
  100 format(' ','Details of Hydrogen dound-free ',A,' at XLM =',
     $           1PE20.13,' Angstroms.')
      call LINER  (1, LUEO)
C     !END
      call BYE ('STERK')
C
      return
      end
