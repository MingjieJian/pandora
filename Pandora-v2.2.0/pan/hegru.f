      subroutine HEGRU
     $(QELSM,IQLYM,KOLEV)
C
C     Rudolf Loeser, 2005 Oct 13
C---- Aborts if KOLEV is bad.
C     (This is version 2 of HEGRU.)
C     !DASH
      save
C     !DASH
      integer IQLYM, KOLEV, LUEO
      character QELSM*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      call HI ('HEGRU')
C     !BEG
      if((QELSM(:3).eq.'H  ').and.(IQLYM.gt.0)) then
        if(KOLEV.ne.1) then
          call MESHED ('HEGRU', 1)
          write (LUEO,100) KOLEV
  100     format(' ','KOLEV =',I12//
     $           ' ','In a Hydrogen run with LYMAN = on, ',
     $               'KOLEV must = 1.'/
     $           ' ','(This is because the procedure for "reserved ',
     $               'emission" [in NAOMI via PABLUM] has not been ',
     $               'generalized for KOLEV > 1.)')
          call ABORT
        end if
      end if
C     !END
      call BYE ('HEGRU')
C
      return
      end
