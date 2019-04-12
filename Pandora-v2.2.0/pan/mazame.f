      subroutine MAZAME
     $(N,NL,Z,XND,RND,VEC,RNDU,KODE,W,IW)
C
C     Rudolf Loeser, 1990 May 15
C---- Computes number density ratios, for diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 RND, RNDU, VEC, W, XND, Z
      integer INDX, IQAN1, IQNRS, IW, J, KODE, KRET, N, NL, NNL
      logical OK
      character LABEL*100, TYPE*3
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
      equivalence (IQQ(272),IQAN1)
      equivalence (IQQ(319),IQNRS)
C     !DASH
      external ONE1, ROWSUM, ARRDIV, SMOOTH, MOVE1, HI, BYE
C
      dimension W(*), IW(*)
C
C               XND(N,NL), RND(N,NL), RNDU(N,NL), Z(N), VEC(N)
      dimension XND(N,*),  RND(N,*),  RNDU(N,*),  Z(*), VEC(*)
C
      data TYPE,INDX /'lin', 0/
C     !EJECT
C
      call HI ('MAZAME')
C     !BEG
      NNL  = N*NL
      KODE = IQNRS
C
      if(IQAN1.gt.0) then
        call ROWSUM     (XND, N, N, 1, NL, VEC)
        do 100 J = 1,NL
          call ARRDIV   (XND(1,J), VEC, RND(1,J), N)
  100   continue
C
        if(KODE.gt.0) then
          call MOVE1    (RND, NNL, RNDU)
          do 102 J = 1,NL
            write (LABEL,101) J
  101       format('Relative number densities (diffusion), ',
     $             'level ',I2)
            call SMOOTH (Z, RND(1,J), N, TYPE, LABEL, INDX, W, IW,
     $                   KRET, OK)
            KODE = KODE+KRET
  102     continue
        end if
C
      else
        call ONE1       (RND, NNL)
        KODE = 0
      end if
C     !END
      call BYE ('MAZAME')
C
      return
      end
