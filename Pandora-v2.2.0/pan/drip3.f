      subroutine DRIP3
     $(N,NL,NO,IETA,RKI,RKO,RKW,RLI,RLO,RP,RS,PIJ,DNRT,NSL,PIS,
     $ KRPIJ,TREFF)
C
C     Rudolf Loeser, 1975 Dec 02
C---- Produces part of the HAWSER printout of Lyman results.
C     !DASH
      save
C     !DASH
      real*8 DNRT, PIJ, PIS, RK, RKI, RKO, RKW, RL, RLI, RLO, RP, RS,
     $       TREFF
      integer I, IETA, KOLEV, KRPIJ, N, NL, NO, NSL
      logical ZDNRT
      character BLANK*1, MARC*1, STAR*1, TITLE*4, XDNRT*12
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external ABJECT, NAUGHTD, LINER, MARKI, SCALP, SHIM, PETPIJ,
     $         HI, BYE
C
C               RKI(N,NL), RLI(N,NL), PIS(N,NL), PIJ(N,NL,NL), DNRT(N),
      dimension RKI(N,*),  RLI(N,*),  PIS(*),    PIJ(*),       DNRT(*),
C
C               RKO(N), RKW(N), RLO(N), RP(N), TREFF(N), RS(N)
     $          RKO(*), RKW(*), RLO(*), RP(*), TREFF(*), RS(*)
C
      call HI ('DRIP3')
C     !BEG
      if(NO.gt.0) then
        call ABJECT    (NO)
C
        call NAUGHTD   (DNRT, 1, N, ZDNRT)
        XDNRT = BLANK
        TITLE = BLANK
        if(.not.ZDNRT) then
          TITLE = 'DNRT'
        end if
        write (NO,100) KOLEV,KOLEV,TITLE
  100   format(' ',6X,20('-'),' RK',I2,' ',20('-'),2X,8('-'),' RL',I2,
     $             ' ',8('-')//
     $         ' ',9X,'New',7X,'Old/New',6X,'WEIGHT',8X,'TR',9X,'New',
     $             7X,'Old/New',8X,'RP',10X,'RS',8X,A4)
        call LINER     (1, NO)
C
        do 103 I = 1,N
          call MARKI   (I, IETA, MARC, STAR, BLANK)
          call SCALP   (RKO(I), RKI(I,KOLEV), RK)
          call SCALP   (RLO(I), RLI(I,KOLEV), RL)
          if(.not.ZDNRT) then
            write (XDNRT,101) DNRT(I)
  101       format(1PE12.4)
          end if
          write (NO,102) I,MARC,RKI(I,KOLEV),RK,RKW(I),TREFF(I),
     $                   RLI(I,KOLEV),RL,RP(I),RS(I),XDNRT
  102     format(' ',I3,A1,1P8E12.4,A12)
          call SHIM    (I, 5, NO)
  103   continue
C
        if(KRPIJ.gt.0) then
          call PETPIJ  (NO, N, NL, PIJ, NSL, PIS)
        end if
C
      end if
C     !END
      call BYE ('DRIP3')
C
      return
      end
