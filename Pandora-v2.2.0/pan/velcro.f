      subroutine VELCRO
     $(NO,IU,IL,ICE,GAMMA,XC,XP,XR,XCL,TAUCL)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Prints PRD data for line IU,IL.
C     !DASH
      save
C     !DASH
      real*8 GAMMA, TAUCL, XC, XCL, XP, XR, ZERO
      integer ICE, IL, IU, KALTG, NO
      character NEW*14, OLD*15
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(74),KALTG)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      data NEW /'Hubeny & Lites'/
      data OLD /'Kneer & Heasley'/
C
      call HI ('VELCRO')
C     !BEG
      if(NO.gt.0) then
        call LINER      (1, NO)
        if(ICE.eq.2) then
          write (NO,101) NEW,GAMMA
        else
          if(XC.ne.ZERO) then
            write (NO,101) OLD,GAMMA,XC,XP,XR
  101       format(' ',A,' P.R.D.',5X,'GAMMA =',F7.3,:,', XC =',
     $                 F7.3,', XP =',F7.3,', XR =',F7.3,5X,A25)
          else
            write (NO,102) OLD,GAMMA,XCL,TAUCL,XP,XR
  102       format(' ',A,' P.R.D.',5X,'GAMMA =',F7.3,', XCL =',
     $                 F7.3,', TAUCL =',1PE10.3,', XP =',0PF7.3,
     $                 ', XR =',F7.3,5X,A25)
          end if
        end if
        if(KALTG.gt.0) then
          write (NO,103)
  103     format(' ','The Cooper, Ballagh & Hubeny formulation of ',
     $               'GMMA(i) is used.')
        end if
      end if
C     !END
      call BYE ('VELCRO')
C
      return
      end
