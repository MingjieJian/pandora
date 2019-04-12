      subroutine MELLA
     $(SEF,SUM,OPAC)
C
C     Rudolf Loeser, 1989 Feb 27
C---- Dumps, for BELLA.
C     (This is version 2 of MELLA.)
C     !DASH
      save
C     !DASH
      real*8 OPAC, SEF, SUM
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
      call HI ('MELLA')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) SUM,SEF,OPAC
  100 format(' ','*** Sum=',1PE20.12,5X,'Stim.emm.=',E20.12,5X,
     $           'Opacity=',E20.12)
C     !END
      call BYE ('MELLA')
C
      return
      end
