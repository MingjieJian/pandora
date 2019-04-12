      subroutine BORLEY
     $(IMAGE,FMIN,FMAX,Z,JQ,JX,KODE,OK)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Initializes diffusion plots.
C     !DASH
      save
C     !DASH
      real*8 FMAX, FMIN, TEN, Z, ZERO, ZMAX, ZMIN
      integer JQ, JX, KODE, NH, NVL, NVS
      logical FOK, OK, ZOK
      character IMAGE*(*), NUMERO*1, PERIOD*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external KINIT, KRIGIA, KLINEC, MONKEY, HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      data NH,NVS,NVL /117, 54, 115/
C     !EJECT
C
      call HI ('BORLEY')
C     !BEG
      if(KODE.eq.1) then
        ZMIN = Z(JQ)
        ZMAX = Z(JX)
      else
        ZMIN = JQ
        ZMAX = JX
      end if
      ZOK = ZMAX.ne.ZMIN
      FOK = FMAX.ne.FMIN
      OK  = ZOK.and.FOK
      if(OK) then
        if(KODE.eq.1) then
          call KINIT    (IMAGE,FMIN,FMAX,ZMAX,ZMIN,NVL,NH,NUMERO,OK)
          if(.not.OK) then
            call KRIGIA (FMIN,FMAX,ZMAX,ZMIN,NVL,NH)
          end if
          if((FMIN.lt.ZERO).and.(FMAX.gt.ZERO)) then
            call KLINEC (IMAGE,ZERO,ZMIN,ZERO,ZMAX,PERIOD,0)
          end if
        else
          call KINIT    (IMAGE,ZMIN,ZMAX,FMIN,FMAX,NVS,NH,NUMERO,OK)
          if(.not.OK) then
            call KRIGIA (ZMIN,ZMAX,FMIN,FMAX,NVS,NH)
          end if
          call MONKEY   (IMAGE,ZMIN,ZMAX,TEN,FMIN,FMAX,PERIOD,1)
          if((FMIN.lt.ZERO).and.(FMAX.gt.ZERO)) then
            call KLINEC (IMAGE,ZMIN,ZERO,ZMAX,ZERO,PERIOD,0)
          end if
        end if
      end if
C     !END
      call BYE ('BORLEY')
C
      return
      end
