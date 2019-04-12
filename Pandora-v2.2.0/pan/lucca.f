      subroutine LUCCA
     $(IMAGE,WAVE,FLUX,NW,LINFLX,OK)
C
C     Rudolf Loeser, 1982 Apr 28
C---- Initializes the plot image, for ACME.
C     (This is version 2 of LUCCA.)
C     !DASH
      save
C     !DASH
      real*8 FLUX, FOUR, ONE, WAVE, XL, XR, YL, YU
      integer NH, NV, NW
      logical LINFLX, OK
      character IMAGE*(*), NUMERO*1, PERIOD*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 5),FOUR  )
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
C     !EJECT
      external NANDI, TILKA, MUNDA, KINIT, KRIGIA, MONKEY, HI, BYE
C
C               WAVE(NW), FLUX(NW)
      dimension WAVE(*),  FLUX(*)
C
      data NV,NH /55, 117/
C
      call HI ('LUCCA')
C     !BEG
      if(LINFLX) then
        call TILKA    (WAVE, NW, XL, XR, OK)
      else
        call NANDI    (WAVE, NW, XL, XR, OK)
      end if
C
      if(OK) then
        call MUNDA    (FLUX, NW, YL, YU)
        call KINIT    (IMAGE, XL, XR, YL, YU, NV, NH, NUMERO, OK)
        if(.not.OK) then
          call KRIGIA (XL, XR, YL, YU, NV, NH)
        end if
C
        call MONKEY   (IMAGE, XL, XR, ONE, YL, YU, PERIOD, 1)
        call MONKEY   (IMAGE, YL, YU, ONE, XL, XR, PERIOD, 2)
      end if
C     !END
      call BYE ('LUCCA')
C
      return
      end
