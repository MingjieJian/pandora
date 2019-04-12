      subroutine PILOT
     $(KODE,BUFF,XLAM,YNT,MUX,MYX,Y,BRIGHT,XLTIT,ISTAR,MODE,EXPAND)
C
C     Rudolf Loeser, 1986 Nov 06
C---- Enters (KODE=1) or fetches (KODE=2) data to/from the Spectrum
C     Summary data buffer.
C     (This is version 2 of PILOT.)
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BUFF, FIVE, XLAM, XLTIT, Y, YNT, ZERO
      integer ISTAR, KODE, MODE, MUX, MYX
      logical EXPAND
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 6),FIVE  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
C               BUFF(common block ICON)
      dimension BUFF(*)
C
      call HI ('PILOT')
C     !BEG
      if(KODE.eq.1) then
        BUFF(1) = XLAM
        BUFF(2) = YNT
        BUFF(3) = MUX
        BUFF(4) = Y
        BUFF(5) = BRIGHT
        BUFF(6) = XLTIT
        BUFF(7) = ISTAR
        BUFF(8) = MODE
        if(EXPAND) then
          BUFF(9) = FIVE
        else
          BUFF(9) = ZERO
        end if
        BUFF(10) = MYX
C
      else if(KODE.eq.2) then
        XLAM = BUFF(1)
        if(XLAM.eq.ZZLARGE) then
          YNT    = ZERO
          MUX    = 0
          Y      = ZERO
          BRIGHT = ZERO
          XLTIT  = ZERO
          ISTAR  = 0
          MODE   = 0
          EXPAND = .true.
          MYX    = 0
        else
          YNT    = BUFF(2)
          MUX    = BUFF(3)
          Y      = BUFF(4)
          BRIGHT = BUFF(5)
          XLTIT  = BUFF(6)
          ISTAR  = BUFF(7)
          MODE   = BUFF(8)
          EXPAND = BUFF(9).eq.FIVE
          MYX    = BUFF(10)
        end if
C
      else
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT ('PILOT', 1)
      end if
C     !END
      call BYE ('PILOT')
C
      return
      end
