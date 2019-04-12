      subroutine GAURA
     $(Z,HND,RHEAB,FMV,N,VM,CVXS,VXS,NVSB,CVSB,VSB,V,HNDV,VNH,NVH,IVNH,
     $ VT,LU,FR,HNDVL,HNDL,VBMB,HEND)
C
C     Rudolf Loeser, 1990 May 07
C---- Recomputes fluid velocities.
C     FR, HEND, HNDVL and HNDL are scratch storage.
C     (This is version 2 of GAURA.)
C     !DASH
      save
C     !DASH
      real*8 CVSB, CVXS, FMV, FR, HEND, HND, HNDL, HNDV, HNDVL, RHEAB,
     $       V, VBMB, VM, VNH, VSB, VT, VXS, Z
      integer ION, IQEXA, IQHSV, IVNH, LU, N, NVH, NVSB
      logical KILROY
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
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ(270),IQHSV)
C     !DASH
      external PENDA, TAMERS, KALMIA, FABIUS, WENDY, HIGRE, HI, BYE
C
C               HEND(N), HND(N), VXS(N), VM(N), VSB(N), FR(N), VBMB(N),
      dimension HEND(*), HND(*), VXS(*), VM(*), VSB(*), FR(*), VBMB(*),
C
C               RHEAB(N), V(N), HNDV(N), VNH(N), FMV(N), HNDL(N), Z(N),
     $          RHEAB(*), V(*), HNDV(*), VNH(*), FMV(*), HNDL(*), Z(*),
C
C               HNDVL(NVH), VT(N)
     $          HNDVL(*),   VT(*)
C     !EJECT
C
      call HI ('GAURA')
C     !BEG
      call PENDA    (ION)
      if((IQHSV.gt.0).or.(ION.ne.0)) then
        call TAMERS (N,Z,HND,RHEAB,VBMB,FMV,FR,HEND,VM)
        call WENDY  (VM,1,N,39,'GAURA')
      end if
      KILROY = .true.
      if(IQEXA.gt.0) then
        call KALMIA (N,CVXS,Z,HND,RHEAB,FR,FMV,KILROY,'VXS',LU,VXS)
      end if
      if(NVSB.gt.0) then
        call KALMIA (N,CVSB,Z,HND,RHEAB,FR,FMV,KILROY,'VSB',LU,VSB)
      end if
      if(IVNH.gt.0) then
        call FABIUS (HNDV,VNH,NVH,HND,V,N,HNDVL,HNDL)
        call HIGRE  (V,VT,N)
      end if
C     !END
      call BYE ('GAURA')
C
      return
      end
