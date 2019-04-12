      subroutine DALI
     $(CHLIM,DUMP,CALLER)
C
C     Rudolf Loeser, 1991 Sep 25
C---- Sets up debug printout for the RHOW calculation in DAISY.
C     (This is version 2 of DALI.)
C     !DASH
      save
C     !DASH
      real*8 CHLIM, ZERO
      integer LUEO, MO
      logical DUMP
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('DALI')
C     !BEG
      DUMP = (CHLIM.lt.ZERO).and.(MO.gt.0)
C
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100)
  100   format(' ','Details of calculation of WW for RHOW. (To omit ',
     $             'printout, set CHLIM .ge. 0.)')
      end if
C     !END
      call BYE ('DALI')
C
      return
      end
