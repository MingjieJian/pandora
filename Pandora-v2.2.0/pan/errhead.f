      subroutine ERRHEAD
     $(LINE)
C
C     Rudolf Loeser, 2002 Jul 12
C---- Enters iteration marker into error file.
C     !DASH
      save
C     !DASH
      integer LUEO, NO
      character LINE*80
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external STARER, LINER, HI, BYE
C
      call HI ('ERRHEAD')
C     !BEG
      if(LUEO.ne.NO) then
        call LINER  (4, LUEO)
        call STARER (LUEO)
        call STARER (LUEO)
        call LINER  (2, LUEO)
C
        write (LUEO,100) LINE
  100   format(' ',A)
C
        call LINER  (2, LUEO)
        call STARER (LUEO)
        call STARER (LUEO)
        call LINER  (4, LUEO)
      end if
C     !END
      call BYE ('ERRHEAD')
C
      return
      end
