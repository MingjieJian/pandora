      subroutine DINGHY
     $(LR,TKRL,RKCL,TAUL,AKL,N,RK,XCBL,TAUK,HE304,M304,J304I)
C
C     Rudolf Loeser, 1975 Sep 10
C---- Computes additional photoionization for the current level,
C     for TIN.
C     !DASH
      save
C     !DASH
      real*8 AKL, DELTA, FAC, HE304, RCK, RK, RKCL, TAUK, TAUL, TKRL,
     $       W304, XCBL
      integer I, I304, IQFIN, ITYPE, J304I, K, KKTAUK, LR, M304, N
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(12),KKTAUK)
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
C     !EJECT
      external COMPD, LAMAR, CONDIV, MOVE1, HI, BYE
C
C               TKRL(LR), TAUL(N,LR), RKCL(LR), XCBK(Miklen), HE304(N),
      dimension TKRL(*),  TAUL(N,*),  RKCL(*),  XCBL(*),      HE304(*),
C
C               AKL(N,LR), RK(N), TAUK(N)
     $          AKL(N,*),  RK(*), TAUK(*)
C
      data W304,DELTA,ITYPE /304.0D0, 1.0D-4, 6/
C
      call HI ('DINGHY')
C     !BEG
C---- Loop over all wavelengths for the current level
      do 100 K = 1,LR
        I304 = 1
        if(J304I.gt.0) then
          call COMPD  (TKRL(K), W304, DELTA, I304)
C         Note: If I304 now = 0, then don't use TAUK, use HE304 ratio
        end if
C
C----   Get TAUK, or HE304 ratio, as needed
        if(I304.ne.0) then
          call LAMAR  (TKRL(K), ITYPE, XCBL)
          call MOVE1  (XCBL(KKTAUK), N, TAUK)
        else
          call MOVE1  (HE304, N, TAUK)
          call CONDIV (HE304(M304), TAUK, N)
        end if
C       Save for later printing
        call MOVE1    (TAUK, N, TAUL(1,K))
C
        RCK = RKCL(K)
C
        do 101 I = 1,N
C----     Set up depth-dependent factor
          if(I304.eq.0) then
            FAC = TAUK(I)
          else if(IQFIN.le.0) then
            FAC = exp(-TAUK(I))
          else
            FAC = exp(-(TAUK(N)-TAUK(I)))
          end if
C----     Compute additional photoionization as function of depth,
          AKL(I,K) = RCK*FAC
C----     and add to total of the current level.
          RK(I) = RK(I)+AKL(I,K)
  101   continue
  100 continue
C     !END
      call BYE ('DINGHY')
C
      return
      end
