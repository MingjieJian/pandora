      subroutine BARLEY
     $(LUNP,DMPN,ITER,LIMIT,HEAD,CALLER,DUMP)
C
C     Rudolf Loeser, 1991 Feb 19
C---- Sets up dumps for the "Special-N1" calculation in the
C     ambipolar diffusion calculation.
C     (This is version 2 of BARLEY.)
C     !DASH
      save
C     !DASH
      integer ITER, LIMIT, LUEO, LUNP, MO
      logical DMPN, DUMP, HEAD
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MESHED, MASHED, LINER, HI, BYE
C     !EJECT
C
      call HI ('BARLEY')
C     !BEG
C---- Turn dump off by default
      DUMP = .false.
C
      if(LUNP.gt.0) then
C----   Regular printout from "Special-N1" is turned on
C
        if(DMPN) then
C----     Detail printout has been requested - turn it on
          DUMP = .true.
        else
C----     Detail printout has not been requested but should be provided
C         anyway if this is the last N1 iteration
          if(ITER.ge.LIMIT) then
            DUMP = .true.
          end if
        end if
C
      end if
C
      if(DUMP) then
        if(.not.HEAD) then
          HEAD = .true.
          call MESHED (CALLER, 2)
        end if
      end if
C
      if(MO.gt.0) then
        if(HEAD) then
          call LINER  (2, LUEO)
        else
          call MESHED ('BARLEY', 2)
        end if
        write (LUEO,100) LUNP,DMPN,DUMP,ITER,LIMIT
  100   format(' ','Status of output controls'/
     $         ' ','LUNP =',I5,', DMPN =',L10,', DUMP =',L10,'; ITER =',
     $             I10,', LIMIT =',I10)
        if(.not.HEAD) then
          call MASHED ('BARLEY')
        end if
      end if
C     !END
      call BYE ('BARLEY')
C
      return
      end
