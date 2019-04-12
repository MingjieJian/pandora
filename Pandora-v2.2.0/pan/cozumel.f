      subroutine COZUMEL
     $(IMAGE,WL,NW,SIG)
C
C     Rudolf Loeser, 1982 May 11
C---- Initializes graph image, for TONY.
C     !DASH
      save
C     !DASH
      real*8 ONE, SIG, WHI, WL, WLO, XL, XR, YH, YL, ZERO
      integer I, NH, NV, NW
      logical GOOD
      character IMAGE*(*), NUMERO*1, PERIOD*1
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external BELOWD, HALT, ABOVED, KINIT, KRIGIA, MONKEY, HI, BYE
C
C               WL(Nmkuse)
      dimension WL(*)
C
      data NV,NH /54, 117/
      data YL,YH /-2.D0, 0.D0/
C     !EJECT
C
      call HI ('COZUMEL')
C     !BEG
C---- Find starting wavelength
      I = 1
  100 continue
        WLO = WL(I)
        if(WLO.ne.SIG) then
          goto 102
        end if
        I = I+1
        if(I.le.NW) then
          goto 100
        end if
        write (MSSLIN(1),101) I,NW
  101   format('No lower wavelength; I =',I12,', NW = ',I12)
        call HALT   ('COZUMEL', 1)
  102 continue
C
C---- Find ending wavelength
      I = NW
  103 continue
        WHI = WL(I)
        if(WHI.ne.SIG) then
          goto 105
        end if
        I = I-1
        if(I.ge.1) then
          goto 103
        end if
        write (MSSLIN(1),104) I
  104   format('No upper wavelength; I =',I12)
        call HALT   ('COZUMEL', 1)
  105 continue
C
C---- Find abscissa limits
      call BELOWD   (WLO, ONE, XL)
      call ABOVED   (WHI, ONE, XR)
C
C---- Initialize graph image
      call KINIT    (IMAGE, XL, XR, YL, YH, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, YL, YH, NV, NH)
      end if
C---- Enter grid lines
      call MONKEY   (IMAGE, XL, XR, ONE, YL, YH, PERIOD, 1)
C     !END
      call BYE ('COZUMEL')
C
      return
      end
