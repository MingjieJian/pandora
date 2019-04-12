      subroutine DOLGAN
     $(LU,IU,IL,R1N,Z,N,DL,WVL,WVNUM,WTAB,K,FRR,MRR,SI,EPCS,EPSS,EPCD,
     $ DI,SF,DF,TF,ICE,QNAME,INT1S,TAU1S,SFC,INT1D,TAU1D,DFC,KILROY,
     $ SII,DII)
C
C     Rudolf Loeser, 1981 Dec 10
C---- Supervises profile output, for LISA.
C     !DASH
      save
C     !DASH
      real*8 DF, DFC, DI, DII, DL, EPCD, EPCS, EPSS, FRR, R1N, SF, SFC,
     $       SI, SII, TAU1D, TAU1S, TF, WTAB, WVL, WVNUM, Z
      integer ICE, IL, INT1D, INT1S, IQEPU, IU, K, LU, LUMR, MRR, N
      logical DISK, KILROY, PRINT
      character QNAME*8, TOT*24
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
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
      equivalence (IQQ(160),IQEPU)
C     !DASH
C     !EJECT
      external LOOT, CAESAR, YARDEN, CHUVASH, HI, BYE
C
C               WTAB(KM), EPCD(N), SI(N,KM), EPCS(KM), EPSS(N), TF(KM),
      dimension WTAB(*),  EPCD(*), SI(*),    EPCS(*),  EPSS(*), TF(*),
C
C               DII(N,KM), WVNUM(KM), INT1S(N,KM), FRR(MRR), SFC(N,KM),
     $          DII(*),    WVNUM(*),  INT1S(*),    FRR(*),   SFC(*),
C
C               TAU1S(N,KM), INT1D(MRR,KM), TAU1D(MRR,KM), DFC(MRR,KM),
     $          TAU1S(*),    INT1D(*),      TAU1D(*),      DFC(*),
C
C               DF(KM), SII(N,KM), DI(N,KM), DL(KM), SF(KM), Z(N)
     $          DF(*),  SII(*),    DI(*),    DL(*),  SF(*),  Z(*)
C
C
      call HI ('DOLGAN')
C     !BEG
      PRINT = (LU.gt.0).and.(K.gt.2)
      if(PRINT.or.(IQEPU.gt.0)) then
        DISK = MRR.gt.0
        call LOOT      (TOT)
        if(PRINT) then
          call CAESAR  (LU,IU,IL,Z,N,WTAB,WVL,K,FRR,MRR,SI,INT1S,TAU1S,
     $                  EPCS,EPSS,EPCD,DI,INT1D,TAU1D,SF,SFC,DF,DFC,TF,
     $                  ICE,R1N,DISK,TOT,KILROY,SII,DII)
        end if
        if(IQEPU.gt.0) then
          call YARDEN  (LUMR,1,'ECLIPSE')
          call CHUVASH (LUMR,QNAME,IU,IL,DISK,TOT,DL,K,R1N,Z,N,FRR,MRR,
     $                  SI,DI,WVL,WVNUM)
          call YARDEN  (LUMR,1,'ECLIPSE')
        end if
      end if
C     !END
      call BYE ('DOLGAN')
C
      return
      end
