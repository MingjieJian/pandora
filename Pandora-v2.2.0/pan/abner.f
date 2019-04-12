      subroutine ABNER
     $(COMU,L,EMU,LF,EMUF)
C
C     Rudolf Loeser, 1993 May 18
C---- Checks validity of COMU, and alters EMU and EMUF if necessary.
C     (This is version 2 of ABNER.)
C     !DASH
      save
C     !DASH
      real*8 COMU, COMUI, EMU, EMUF, ONE, ZERO
      integer L, LF, LUEO
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  MUSHED, MASHED, HI, BYE
      intrinsic min, max
C
C               EMU(L), EMUF(LF)
      dimension EMU(*), EMUF(*)
C
      call HI ('ABNER')
C     !BEG
      KILROY = .true.
C
      if(COMU.ne.ZERO) then
        if((COMU.lt.ZERO).or.(COMU.gt.ONE)) then
          COMUI = COMU
          COMU  = max(min(COMU,ONE),ZERO)
C
          call MUSHED ('ABNER', 3, KILROY)
          write (LUEO,100) COMUI,COMU
  100     format(' ','Input value of COMU =',1PE12.4,' is invalid; ',
     $               'has been changed to COMU =',E12.4)
        end if
      end if
C
      if(COMU.ne.ZERO) then
        if((LF.gt.1).or.(EMUF(1).ne.COMU).or.(EMU(1).ne.COMU)) then
          LF = 1
          L  = 1
          EMUF(1) = COMU
          EMU(1)  = COMU
C
          call MUSHED ('ABNER', 3, KILROY)
          write (LUEO,101) COMU
  101     format(' ','Since COMU =',1PE12.4,', the values of LF and ',
     $               'L must both = 1, and EMUF(1) and EMU(1) must ',
     $               'both = COMU.'//
     $           ' ','Input values have been altered accordingly.')
        end if
      end if
C
      if(.not.KILROY) then
        call MASHED   ('ABNER')
      end if
C     !END
      call BYE ('ABNER')
C
      return
      end
