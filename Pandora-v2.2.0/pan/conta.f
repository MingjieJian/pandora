      subroutine CONTA
     $(N,DEE,DEEL,JQ,JX,YBOT,YTOP)
C
C     Rudolf Loeser, 1998 Oct 30
C---- Sets plot limits, for ACTON.
C     Only nonzero values of d are significant.
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, DMAX, DMIN, TEN, YBOT, YTOP
      integer JMAX, JMIN, JQ, JX, K, N
      logical ALLSML
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external MAKO, MOKA, HI, BYE
C
C               DEE(4,5,N), DEEL(4,5,N)
      dimension DEE(4,5,*), DEEL(4,5,*)
C     !EJECT
C
      call HI ('CONTA')
C     !BEG
C---- Find extrema of logs of d
      DMIN = ZZLARGE
      DMAX = ZZSMALL
      do 100 K = 1,N
        call MAKO (DEE(1,1,K),DEEL(1,1,K),DMIN,DMAX)
  100 continue
C
C---- Set top limit of Y-axis
      JMAX = DMAX
      YTOP = JMAX+1
C---- Set bottom limit of Y-axis
      JMIN = DMIN
      YBOT = JMIN-1
      if((YTOP-YBOT).gt.TEN) then
C----   Allow only at most full decades
        YBOT = YTOP-TEN
      end if
C
C---- Set X-axis limit indices
      JQ = 1
      JX = N
C---- Move right X-axis limit inward to skip small values
      do 101 K = N, 1,-1
        call MOKA (DEE(1,1,K),DEEL(1,1,K),YBOT,ALLSML)
        if(.not.ALLSML) goto 102
        JX = JX-1
  101 continue
  102 continue
C---- Move left X-axis limit inward to skip small values
      do 103 K = 1,JX,+1
        call MOKA (DEE(1,1,K),DEEL(1,1,K),YBOT,ALLSML)
        if(.not.ALLSML) goto 104
        JQ = JQ+1
  103 continue
  104 continue
C     !END
      call BYE ('CONTA')
C
      return
      end
