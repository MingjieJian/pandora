      subroutine DARLING
     $(KILROY,CALLER,AMASS,I,J,TE,HN1,EQNJ)
C
C     Rudolf Loeser, 1991 May 07
C---- Prints debug output header, for FRANCIS.
C     !DASH
      save
C     !DASH
      real*8 AMASS, EQNJ, HN1, TE
      integer I, J, LUEO
      logical KILROY
      character CALLER*(*)
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
      call HI ('DARLING')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED (CALLER, 2)
        write (LUEO,100) I,AMASS,TE,HN1
  100   format(' ','Debug printout for ionization rates due to ',
     $             'collisions with Hydrogen atoms'//
     $         ' ','at depth index I = ICHDP =',I3,5X,'(for no debug ',
     $             'printout, set ICHDP = 0).'/
     $         ' ','mass=',1PE16.8,5X,'TE=',E16.8,5X,'NH1=',E16.8)
      end if
C
      call LINER    (1, LUEO)
      write (LUEO,101) J,EQNJ
  101 format(' ','Level',I5,10X,'n*=',1PE14.7)
      call LINER    (1, LUEO)
C     !END
      call BYE ('DARLING')
C
      return
      end
