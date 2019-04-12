      subroutine DAMPIER
     $(KILROY,CALLER,AMASS,I,TE,HN1,IU,EQNU,IL,EQNL)
C
C     Rudolf Loeser, 1991 May 07
C---- Prints debug output header, for DAMALA.
C     !DASH
      save
C     !DASH
      real*8 AMASS, EQNL, EQNU, HN1, TE
      integer I, IL, IU, LUEO
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
      call HI ('DAMPIER')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED (CALLER, 2)
        write (LUEO,100) I,AMASS,TE,HN1
  100   format(' ','Debug printout for transition rates due to ',
     $             'collisions with Hydrogen atoms'//
     $         ' ','at depth index I = ICHDP =',I3,5X,
     $             '(for no debug printout, set ICHDP = 0).'/
     $         ' ','mass=',1PE16.8,5X,'TE=',E16.8,5X,'NH1=',E16.8)
      end if
C
      call LINER    (1, LUEO)
      write (LUEO,101) IU,IL,EQNU,EQNL
  101 format(' ','Transition (',I2,'/',I2,')',10X,'n*(U)=',1PE14.7,
     $           5X,'n*(L)=',E14.7)
      call LINER    (1, LUEO)
C     !END
      call BYE ('DAMPIER')
C
      return
      end
