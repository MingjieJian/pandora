      subroutine HAMRI
     $(N,NL,NT,INDRN,X,IX,W,IW,HND,RABD,ABDEL,XNK,XND,FRN,FOLD,FCE,
     $ IMG,NO)
C
C     Rudolf Loeser, 1999 Jun 11
C---- Normalizes input number densities.
C     !DASH
      save
C     !DASH
      real*8 ABD, ABDEL, FCE, FOLD, FRN, HND, RABD, W, X, XND, XNK
      integer IMG, INDRN, IQCEF, IW, IX, KFCE, KOUNT, N, NL, NO, NT
      logical ZND, ZNK, lummy
      character QELSM*8
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
      equivalence (RZQ(  6),ABD  )
      equivalence (QZQ(  2),QELSM)
C
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
      equivalence (IQQ(328),IQCEF)
C     !DASH
C     !EJECT
      external ELINOR, LUNA, NAUGHTD, LINER, DVECOUT, RAVI, DASHER,
     $         ONE1, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               HND(N), RABD(N), FCE(N,NT), XNK(N), XND(N,NL), FOLD(N),
      dimension HND(*), RABD(*), FCE(*),    XNK(*), XND(N,*),  FOLD(*),
C
C               FRN(N), IMG(N), ABDEL(N)
     $          FRN(*), IMG(*), ABDEL(*)
C
      data KFCE /0/
C
      call HI ('HAMRI')
C     !BEG
      if(IQCEF.le.0) then
C       Make sure FCE = 1
        call ONE1        (FCE, (N*NT))
      end if
C
      if(INDRN.gt.0) then
        call NAUGHTD     (XNK, 1, N     , ZNK)
        call NAUGHTD     (XND, 1, (N*NL), ZND)
        if((.not.ZNK).or.(.not.ZND)) then
C
          call LUNA      (X, QELSM, ABD, ABDEL)
          KOUNT = 0
          call ELINOR    (N, NL, HND, RABD, ABDEL, FRN, XNK, XND,
     $                    KOUNT, FOLD, IMG)
C
          if(NO.gt.0) then
            call LINER   (1, NO)
            call DASHER  (NO)
            call LINER   (1, NO)
            write (NO,100) INDRN,N,NL,ZNK,ZND
  100       format(' ','Input number densities were renormalized ',
     $                 'a la  [ 99 Jun 11 ] .',10X,3I6,2L6)
            call DVECOUT (NO, FRN, N, 'FRN')
            call DASHER  (NO)
          end if
C
          if(KOUNT.gt.0) then
C           (Uses FCE!)
            call RAVI    (X, IX, W, IW, XND, XND, lummy, KFCE, lummy)
          end if
C
        end if
C
      else
        call ONE1        (FRN, N)
      end if
C     !END
      call BYE ('HAMRI')
C
      return
      end
