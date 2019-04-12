      subroutine PRISCUS
     $(SINT,RADI,NRAD,NP,TOP,BOT,XLL,XRL,IS,KODE)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Computes graph limits, for AMASIA.
C     Returns with KODE=1 if all seems OK, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 BOT, RADI, SINT, TOP, XLL, XRL, YLL, YUL, ZERO
      integer IS, KODE, NP, NRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external CHROME, TITAN, HI, BYE
C
C               RADI(NRAD), SINT(NRAD,NP)
      dimension RADI(*),    SINT(*)
C
      call HI ('PRISCUS')
C     !BEG
      YUL = -ZZLARGE
      YLL = +ZZLARGE
      call CHROME   (NP, NRAD, SINT, 1, ZERO, YUL, YLL)
      if((YUL.gt.YLL).and.(YUL.gt.ZERO).and.(YLL.gt.ZERO)) then
        call TITAN  (YLL, YUL, BOT, TOP)
        if(RADI(1).gt.ZERO) then
          IS = 1
        else
          IS = 2
        end if
C
        XLL  = log10(RADI(IS))
        XRL  = log10(RADI(NRAD))
        KODE = 1
      else
        KODE = 0
      end if
C     !END
      call BYE ('PRISCUS')
C
      return
      end
