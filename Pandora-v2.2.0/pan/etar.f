      subroutine ETAR
     $(KILROY,CALLER,IU,IL,I,TE,HN1,FC,AUL,AMASS,EQNL,EQNU,Y,H,D,W)
C
C     Rudolf Loeser, 1991 May 15
C---- Prints debug printout for DERVENI.
C     !DASH
      save
C     !DASH
      real*8 AMASS, AUL, D, EQNL, EQNU, FC, H, HN1, TE, W, Y
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
      call HI ('ETAR')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED (CALLER, 2)
        write (LUEO,100) I,AMASS,TE,HN1
  100   format(' ','Debug printout for transition rates due to ',
     $             'collisions with Hydrogen atoms'//
     $         ' ','at depth index I = ICHDP =',I3,5X,'(for no debug ',
     $             'printout set ICHDP = 0).'/
     $         ' ','Mass=',1PE16.8,5X,'TE=',E16.8,5X,'NH1=',E16.8)
      end if
C
      call LINER    (1, LUEO)
      write (LUEO,101) IU,IL,EQNL,EQNU,AUL,FC,Y,H,W,D
  101 format(' ','Transition (',I2,'/',I2,')',5X,'n*(L)=',1PE14.7,5X,
     $           'n*(U)=',E14.7,5X,'AUL=',E16.8,5X,'FC=',E16.8/
     $       ' ','Y(I)=',E16.8,5X,'H(I)=',E16.8,5X,'W=',E16.8,5X,'D=',
     $           E16.8)
C     !END
      call BYE ('ETAR')
C
      return
      end
