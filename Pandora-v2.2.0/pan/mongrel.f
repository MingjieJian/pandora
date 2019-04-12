      subroutine MONGREL
     $(KBNDS,KINOUT,KBINN,KBOUT)
C
C     Rudolf Loeser, 1998 Jul 16
C---- Sets up boundary condition switches, for diffusion.
C     !DASH
      save
C     !DASH
      integer KBINN, KBNDS, KBOUT, KINOUT
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('MONGREL')
C     !BEG
      KBINN = 0
      KBOUT = 0
C
      if(KBNDS.eq.1) then
        if(KINOUT.eq.1) then
          KBINN = 1
        else if(KINOUT.eq.2) then
          KBOUT = 1
        end if
      end if
C
      if((KBINN+KBOUT).gt.1) then
        write (MSSLIN(1),100) KBINN,KBOUT
  100   format('KBINN =',I12,', KBOUT =',I12,'; they may not both be ',
     $         'greater than 0.')
        call HALT ('MONGREL',1)
      end if
C     !END
      call BYE ('MONGREL')
C
      return
      end
