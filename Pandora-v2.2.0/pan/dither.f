      subroutine DITHER
     $(NO,N,NRAD,IMAGE,TAUIJ,LABIJ,TE,REMARK)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Plots Te vs. Tau, for each transition.
C     !DASH
      save
C     !DASH
      real*8 BOT, TAUIJ, TE, THI, TLO, TOP, XL, XMODU, XR
      integer IHI, ILO, IMAX, IMIN, KNT, LABIJ, N, NO, NRAD
      logical GOOD
      character IMAGE*(*), NUMERO*1, REMARK*8
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
C     !DASH
C     !EJECT
      external TITANIA, MINMAXD, BELOWD, ABOVED, KPRINT, KINIT, SAMSON,
     $         KRIGIA, DELILAH, ABJECT, LINER, HI, BYE
C
C               TE(N), TAUIJ(N,NRAD), LABIJ(NRAD)
      dimension TE(*), TAUIJ(*),      LABIJ(*)
C
      data XMODU,TLO,THI /1.D3, 1.D-3, 1.D1/
C
      call HI ('DITHER')
C     !BEG
C---- Establish ordinate limits
      call TITANIA    (N, TAUIJ, NRAD, TLO, THI, ILO, IHI)
      KNT = IHI-(ILO-1)
      call MINMAXD    (TE(ILO), 1, KNT, IMIN, IMAX)
      call BELOWD     (TE(IMIN+(ILO-1)), XMODU, BOT)
      call ABOVED     (TE(IMAX+(ILO-1)), XMODU, TOP)
      if(TOP.gt.BOT) then
C----   Initialize control parameters
        JXLOBA = 1
        JXHIBA = 117
        JYLOBA = 1
        JYHIBA = 51
        NTHRBA = 5*NRAD
        NPOIBA = 0
C----   Initialize plot image
        XL = log10(TLO)
        XR = log10(THI)
        call KINIT    (IMAGE, XL, XR, BOT, TOP, JYHIBA, JXHIBA, NUMERO,
     $                 GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, BOT, TOP, JYHIBA, JXHIBA)
        end if
C----   Enter line segments
        call DELILAH  (N, NRAD, IMAGE, TAUIJ, TE, TLO, THI)
C----   Print plot header and image
        if(NPOIBA.gt.NTHRBA) then
          call ABJECT (NO)
          write (NO,100) REMARK
  100     format(' ','Graph of TE vs. log(Optical Depth), for ',
     $               'radiative transitions, ',A8,' graph.')
          call LINER  (1, NO)
          call KPRINT (IMAGE, NO)
C----     Print labels
          call SAMSON (NO, NRAD, LABIJ)
        end if
      end if
C     !END
      call BYE ('DITHER')
C
      return
      end
