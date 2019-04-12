      subroutine KUNTUR
     $(KAMB,KVLG,N,VM,VH,V1,V2,VXI,VXS,VV1,VV2,VADD)
C
C     Rudolf Loeser, 1990 May 1
C---- Computes VADD, and updates VXS (if needed).
C     (This is version 2 of KUNTUR.)
C     !DASH
      save
C     !DASH
      real*8 V1, V2, VADD, VH, VM, VV1, VV2, VXI, VXS
      integer IQVLS, KAMB, KVLG, N
      logical VZERO
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
      equivalence (IQQ(265),IQVLS)
C     !DASH
      external MOVE1, TRINCA, ARRADD, NEGATE, NAUGHTD, ZERO1, HI, BYE
C
C               V1(N), V2(N), VXI(N), VXS(N), VADD(N), VV1(N), VV2(N),
      dimension V1(*), V2(*), VXI(*), VXS(*), VADD(*), VV1(*), VV2(*),
C
C               VM(N), VH(N)
     $          VM(*), VH(*)
C     !EJECT
C
      call HI ('KUNTUR')
C     !BEG
      call ZERO1     (VV1,N)
      call ZERO1     (VV2,N)
C
      if((KAMB.ge.1).and.(KAMB.le.3)) then
        if(KAMB.eq.2) then
          call MOVE1 (V1,N,VV1)
        else if (KAMB.eq.3) then
          call MOVE1 (V2,N,VV1)
        else
          call MOVE1 (VH,N,VV1)
        end if
        call NEGATE  (VV1,N)
        call TRINCA  (VV1,N)
      end if
C
      if(KVLG.gt.0) then
        call MOVE1   (VM,N,VV2)
        call NEGATE  (VV2,N)
      end if
C
      call ARRADD    (VV1,VV2,VADD,N)
C
      call NAUGHTD   (VV1,1,N,VZERO)
      if((.not.VZERO).and.(IQVLS.gt.0)) then
        call ARRADD  (VXI,VV1,VXS,N)
      end if
C     !END
      call BYE ('KUNTUR')
C
      return
      end
