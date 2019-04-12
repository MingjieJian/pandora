      subroutine JONQUIL
     $(BDJ,BDR,BDQ,BDS,BDIJ,SSTAR,N,NL,NT,KIJ,NO,W)
C
C     Rudolf Loeser, 1980 OCT 31
C---- Prints full BD sets, for TULIP.
C     (This is version 2 of JONQUIL.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, BDJ, BDQ, BDR, BDS, SSTAR, W
      integer IJECT, IN, IQARD, IQBRD, IS, ITERM, KIJ, KNL, KNT, MOX, N,
     $        NL, NO, NT
      logical BZERO, SZERO
      character TIT*34
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 15),IQARD)
      equivalence (IQQ(311),IQBRD)
C     !DASH
C     !EJECT
      external IMRE, EDGY, MEETOO, YOUTOO, SCRIBE, NAUGHTD, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               BDJ(N,NL), BDR(N,NL), KIJ(NL,NL), BDIJ(N,NL), BDS(N,NT),
      dimension BDJ(*),    BDR(*),    KIJ(*),     BDIJ(*),    BDS(*),
C
C               SSTAR(N,NT), BDQ(N,NL)
     $          SSTAR(*),    BDQ(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),ITERM )
C
      data TIT /'Ratios of Departure Coefficients, '/
C
      call HI ('JONQUIL')
C     !BEG
      if(NO.gt.0) then
        KNL = N*NL
        KNT = N*NT
        IJECT = 0
C
        call NAUGHTD    (BDJ, 1, KNL, BZERO)
        if(.not.BZERO) then
          call EDGY     (NO, IJECT)
          write (NO,100) TIT
  100     format(' ','BDJ: ',A34,'from JBAR.')
          call MEETOO   (BDJ, 1, N, NL, NO)
        end if
C
        call NAUGHTD    (BDR, 1, KNL, BZERO)
        if(.not.BZERO) then
          call EDGY     (NO, IJECT)
          write (NO,101) TIT
  101     format(' ','BDR: ',A34,'from RHO (and JBAR).')
          call MEETOO   (BDR, 1, N, NL, NO)
        end if
C     !EJECT
        call NAUGHTD    (BDQ, 1, KNL, BZERO)
        if(.not.BZERO) then
          call EDGY     (NO, IJECT)
          write (NO,102) TIT
  102     format(' ','BDQ: ',A34,'from CHI.')
          call MEETOO   (BDQ, 1, N, NL, NO)
        end if
C----
        call NAUGHTD    (BDS, 1, KNT, BZERO)
        if(.not.BZERO) then
C         (Get W allotment)
          call IMRE     (IN, IS, MOX, 'JONQUIL')
C
          call EDGY     (NO, IJECT)
          write (NO,103) TIT
  103     format(' ','BDS: ',A34,'from S.')
          call YOUTOO   (BDIJ, BDS, 1, N, N, NL, KIJ, NO, W(ITERM))
C
C         (Give back W allotment)
          call WGIVE    (W, 'JONQUIL')
        end if
C
        if((IQARD.gt.0).or.(IQBRD.gt.0)) then
          call NAUGHTD  (SSTAR, 1, KNT, SZERO)
          if(.not.SZERO) then
            call EDGY   (NO, IJECT)
            write (NO,104)
  104       format(' ','S* (for BDJ)')
            call SCRIBE (SSTAR, 'NT', KIJ, 1, N, N, NL, NO, 1)
          end if
        end if
      end if
C     !END
      call BYE ('JONQUIL')
C
      return
      end
