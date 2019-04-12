      subroutine PUFF
     $(XF,YF,NF,XT,YT,NT,W1,W2,QNAME,I1,I2)
C
C     Rudolf Loeser, 1980 Dec 16
C---- Interpolates from YF(XF) to YT(XT), for CREAM.
C     (This is version 2 of PUFF.)
C     !DASH
      save
C     !DASH
      real*8 ALIM, W1, W2, XF, XT, YF, YT, ZERO
      integer I, I1, I2, IQIXT, KODE, MODE, NF, NT
      logical NYF
      character QNAME*8
C     !COM
C---- EXTRSW      as of 1992 Apr 01
      real*8      XTRALMU,XTRALML
      common      /EXTRSW/ XTRALMU,XTRALML
C     Parameters controlling the extra(inter)polation of input data
C     from an auxiliary Z-scale to the Z-scale of the run.
C     XTRALMU is the allowed upper limit for extrapolated values;
C     XTRALML is the allowed lower limit for extrapolated values.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(297),IQIXT)
C     !DASH
C     !EJECT
      external MOVE1, NAUGHTD, ZERO1, DERE, AGLAURA, HI, BYE
C
C               XF(NF), YF(NF), XT(NT), YT(NT), W1(NF), W2(NT)
      dimension XF(*),  YF(*),  XT(*),  YT(*),  W1(*),  W2(*)
C
      call HI ('PUFF')
C     !BEG
      if(NF.le.0) then
        call MOVE1        (YF, NT, YT)
      else
C
        MODE = 1
        if(IQIXT.gt.0) then
          MODE = 2
        end if
        call NAUGHTD      (YF, 1, NF, NYF)
        if(NYF) then
          call ZERO1      (YT, NT)
        else
          KODE = 0
          do 100 I = 1,NF
            if(YF(I).le.ZERO) then
              KODE = 1
              go to 101
            else
              W1(I) = log(YF(I))
            end if
  100     continue
  101     continue
C
          if(KODE.eq.0) then
            call DERE     (XF, 1, W1, 1, NF, XT, 1, W2, 1, NT, MODE)
            ALIM = log(XTRALMU)
            do 102 I = 1,NT
              if(W2(I).le.ALIM) then
                YT(I) = exp(W2(I))
              else
                YT(I) = XTRALMU
              end if
  102       continue
          else
            call DERE     (XF, 1, YF, 1, NF, XT, 1, YT, 1, NT, MODE)
          end if
C
          call AGLAURA    (XT, YT, NT, XF, YF, NF, QNAME, I1, I2)
        end if
C
      end if
C     !END
      call BYE ('PUFF')
C
      return
      end
