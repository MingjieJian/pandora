      subroutine CHOOSE
     $(TAUK,EP1,N,LNLIM,EXLYM,TGLYM,IETA,KASE)
C
C     Rudolf Loeser, 1969 Sep 26
C---- Looks at large TAUK.
C     !DASH
      save
C     !DASH
      real*8 EP1, EXLYM, TAUK, TGLYM
      integer I, IETA, IQFIN, IQSFS, KASE, LNLIM, N
      logical Q
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external HI, BYE
C
C               TAUK(N), EP1(N)
      dimension TAUK(*), EP1(*)
C     !EJECT
C
      call HI ('CHOOSE')
C     !BEG
      KASE = 1
      IETA = N
C
      if(IQSFS.le.0) then
C
        if(IQFIN.le.0) then
          do 100 I = 1,N
            if(TAUK(I).gt.TGLYM) then
              IETA = I
              if((EP1(I)**2)*TAUK(I).gt.EXLYM) then
                goto 101
              end if
            end if
  100     continue
        end if
C
  101   continue
C
        Q = ((TAUK(2).ge.EXLYM) .or. (LNLIM.le.1) .or. (IETA.lt.LNLIM))
C
        if(Q) then
          KASE = 2
          IETA = 0
        else if(IETA.ne.N) then
          KASE = 3
        end if
C
      end if
C     !END
      call BYE ('CHOOSE')
C
      return
      end
