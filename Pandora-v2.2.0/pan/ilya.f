      subroutine ILYA
     $(XNUK,WNUK,NSL,XNU,WNU)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Sets up frequency values for ions other than Hydrogen.
C     (This is version 3 of ILYA.)
C     !DASH
      save
C     !DASH
      real*8 CON21, WNU, WNUK, XNU, XNUK, ZERO
      integer J, LUEO, NSL
      logical KILROY, KWNU, KXNU
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
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
C     !EJECT
      external RIGEL, LINER, NAUGHTD, VECOUT, ABORT, MUSHED, HI, BYE
C
C               XNU(NSL), WNU(NSL)
      dimension XNU(*),   WNU(*)
C
      call HI ('ILYA')
C     !BEG
      KILROY = .true.
      call RIGEL      (21, CON21)
C
      if(XNUK.le.ZERO) then
        if(WNUK.le.ZERO) then
          call MUSHED ('ILYA', 1, KILROY)
          write (LUEO,100) XNUK,WNUK
  100     format(' ','XNUK =',1PE16.8,5X,'WNUK =',E16.8)
          call LINER  (1, LUEO)
          write (LUEO,101)
  101     format(' ','Both bad!')
        else
          XNUK = CON21*WNUK
        end if
      end if
C
      call NAUGHTD      (XNU, 1, NSL, KXNU)
      call NAUGHTD      (WNU, 1, NSL, KWNU)
      if(KXNU.and.KWNU) then
        call MUSHED     ('ILYA', 1, KILROY)
        call VECOUT     (LUEO, XNU, NSL, 'NU' )
        call VECOUT     (LUEO, WNU, NSL, 'WNU')
        call LINER      (1, LUEO)
        write (LUEO,101)
      end if
C
      if(XNU(1).ne.ZERO) then
        XNU(1) = ZERO
      end if
      do 103 J = 2,NSL
        if(XNU(J).le.ZERO) then
          if(WNU(J).le.ZERO) then
            call MUSHED ('ILYA', 1, KILROY)
            write (LUEO,102) J,XNU(J),WNU(J)
  102       format(' ','Level',I4,5X,'NU =',1PE16.8,5X,'WNU =',E16.8)
            call LINER  (1, LUEO)
            write (LUEO,101)
          else
            XNU(J) = CON21*WNU(J)
          end if
        end if
  103 continue
C
      if(.not.KILROY) then
        call ABORT
      end if
C     !END
      call BYE ('ILYA')
C
      return
      end
