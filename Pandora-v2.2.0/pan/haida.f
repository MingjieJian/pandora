      subroutine HAIDA
     $(KOUNT,KOUNTS,KOUNTF,LABEL,KILROY,CALLER)
C
C     Rudolf Loeser, 1987 Mar 02
C---- Prints messages, if appropriate,
C     based on the values of the change-counters set by RUBY (q.v.).
C     (This is version 3 of HAIDA.)
C     !DASH
      save
C     !DASH
      integer KOUNT, KOUNTF, KOUNTS, LUEO
      logical KILROY
      character CALLER*(*), LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, HI, BYE
C
      call HI ('HAIDA')
C     !BEG
      if(KOUNT.gt.0) then
        if(KILROY) then
          KILROY = .false.
          call MESHED (CALLER, 2)
        end if
C
        call LINER    (1, LUEO)
        write (LUEO,100) LABEL
  100   format(' ','Inappropriate values of ',A,' were found;')
C
        if(KOUNTS.gt.0) then
          write (LUEO,101) '-3'
  101     format(' ','they were changed to ',A)
        else if(KOUNTF.gt.0) then
          write (LUEO,101) '-1'
        else
          write (LUEO,101) 'huh?'
        end if
C
      end if
C     !END
      call BYE ('HAIDA')
C
      return
      end
