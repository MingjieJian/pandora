      subroutine GUEST
     $(N,NL,FCE,KIJ,SIJ,CIJ)
C
C     Rudolf Loeser, 2004 Apr 02
C---- Applies FCE to SIJ, to obtain CIJ.
C     !DASH
      save
C     !DASH
      real*8 CIJ, ENHANCE, FCE, SIJ
      integer I, IJ, IQCEF, ITAU, J, KIJ, N, NL
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
      external INDXIJ, CHANCE, MOVE1, HI, BYE
C
C               FCE(N,NT), SIJ(N,NL**2), CIJ(N,NL**2), KIJ(NL,NL)
      dimension FCE(*),    SIJ(N,*),     CIJ(N,*),     KIJ(*)
C
      call HI ('GUEST')
C     !BEG
      if(IQCEF.gt.0) then
C
        do 102 ITAU = 1,N
          do 101 J = 1,NL
            do 100 I = 1,NL
              if(I.ne.J) then
C
                if(I.gt.J) then
                  call CHANCE (ITAU, I, J, KIJ, FCE, ENHANCE)
                else
                  call CHANCE (ITAU, J, I, KIJ, FCE, ENHANCE)
                end if
                call INDXIJ   (I, J, IJ)
                CIJ(ITAU,IJ) = ENHANCE*SIJ(ITAU,IJ)
C
              end if
  100       continue
  101     continue
  102   continue
C
      else
        call MOVE1            (SIJ, (N*NL*NL), CIJ)
      end if
C     !END
      call BYE ('GUEST')
C
      return
      end
