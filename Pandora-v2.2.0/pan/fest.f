      subroutine FEST
     $(DUMP,ITER)
C
C     Rudolf Loeser, 1998 May 27
C---- Prints a marker, for CARAMBA.
C     (This is version 3 of FEST.)
C     !DASH
      save
C     !DASH
      integer ITER, LUEO
      logical DUMP
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
      call HI ('FEST')
C     !BEG
      if(DUMP) then
        call LINER (2, LUEO)
        write (LUEO,100) ITER
  100   format(' ','>>>>> End of {N1,NK,ND}-recalculation output for ',
     $             'N1-iter: ',I3,'.')
      end if
C     !END
      call BYE ('FEST')
C
      return
      end
