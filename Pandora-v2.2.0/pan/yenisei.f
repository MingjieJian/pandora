      subroutine YENISEI
     $(N,Z,MRR,FRR,NW,WAVES,WVNUM,WTAB,TF,SF,DF,SCON,OPAC,XMULT,LTYPE,
     $ XLTIT,LU,LINFLX,LINK,XLB3,W,IJECT)
C
C     Rudolf Loeser, 1981 Sep 15
C---- Computes total Continuum Flux, using spherical coordinates.
C     !DASH
      save
C     !DASH
      real*8 DF, FRR, OPAC, R1N, SCON, SF, TF, W, WAVES, WTAB, WVNUM,
     $       XLB3, XLTIT, XMULT, Z, ZERO
      integer I, IAD, IAS, IDR, IDRP, IDX, IEM, IFF, IFX, IGX, IJECT,
     $        IN, IQESD, IR, IS, ITX, IXI, LINK, LTYPE, LU, MOX, MRR, N,
     $        NW
      logical DISK, DUMP, LINFLX, SPHERE
      character TIT*4
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
      equivalence (RZQ( 23),R1N  )
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
      equivalence (IQQ( 40),IQESD)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external IRKUTSK, TOBOSA, ELECTRA, MELILOT, ACAMAS, PYXIE, ADAPA,
     $         RANI, APEX, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension W(*)
C
C               TF(Nmkuse), WTAB(Nmkuse), WVNUM(Nmkuse), WAVES(Nmkuse),
      dimension TF(*),      WTAB(*),      WVNUM(*),      WAVES(*),
C
C               XLTIT(Nmkuse), SCON(N,Nmkuse), LTYPE(Nmkuse), FRR(MRR),
     $          XLTIT(*),      SCON(N,*),      LTYPE(*),      FRR(*),
C
C               OPAC(N,Nmkuse), DF(Nmkuse), SF(Nmkuse), XMULT(Nmkuse),
     $          OPAC(N,*),      DF(*),      SF(*),      XMULT(*),
C
C               XLB3(Li3len), Z(N)
     $          XLB3(*),      Z(*)
C
      dimension IN(12)
      equivalence
     $(IN( 1),IR    ),(IN( 2),IAS   ),(IN( 3),IDRP  ),(IN( 4),IDR   ),
     $(IN( 5),IAD   ),(IN( 6),IFX   ),(IN( 7),IGX   ),(IN( 8),IEM   ),
     $(IN( 9),IFF   ),(IN(10),IXI   ),(IN(11),IDX   ),(IN(12),ITX   )
C
      data TIT    /'Flux'/
      data SPHERE /.true./
C
      call HI ('YENISEI')
C     !BEG
C     (Get, and allocate, W allotment)
      call IRKUTSK (IN, IS, MOX, 'YENISEI')
C
      DISK = MRR.gt.0
      DUMP = (.not.LINFLX).and.(LU.gt.0).and.(IQESD.gt.0)
C     !EJECT
C---- Compute integration weights
      call ELECTRA    (N, R1N, Z, W(IR), W(IAS))
      if(DISK) then
        call MELILOT  (MRR, FRR, R1N, W(IDRP), W(IDR), W(IAD))
      end if
      if(DUMP) then
        call MESHED   ('YENISEI', 2)
        call ACAMAS   (N, Z, R1N, MRR, FRR, W(IR), W(IAS), W(IDRP),
     $                 W(IDR), W(IAD), TIT)
      end if
C
C---- Loop over all wavelenghts
      do 100 I = 1,NW
        if(DUMP) then
          call ADAPA  (WAVES(I), OPAC(1,I), SCON(1,I), N, TIT)
        end if
C----   Compute Shell Intensity and Flux
        call RANI     (N, R1N, Z, OPAC(1,I), SCON(1,I), W(IXI),
     $                 W(IDX), W(IFX), W(IGX), W(ITX), WAVES(I), 2,
     $                 DUMP)
        call TOBOSA   (N, W(IAS), W(IXI), W(IFF), SF(I))
C----   Compute Disk Intensity and Flux
        if(DISK) then
          call PYXIE  (FRR, MRR, Z, OPAC(1,I), SCON(1,I), N, W(IDX),
     $                 W(IFX), W(IGX), W(ITX), W(IEM), WAVES(I), TIT,
     $                 W, DUMP)
          call TOBOSA (MRR, W(IAD), W(IEM), W(IFF), DF(I))
        else
          DF(I) = ZERO
        end if
C----   Total Flux
        TF(I) = SF(I)+DF(I)
  100 continue
C
C---- Print, save results for later use, and plot graphs
      call APEX       (LU, NW, WAVES, WVNUM, WTAB, XMULT, LTYPE,
     $                 SPHERE, TF, SF, XLTIT, IJECT, 1, LINFLX, LINK,
     $                 XLB3, W)
C
      if(DUMP) then
        call MASHED   ('YENISEI')
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'YENISEI')
C     !END
      call BYE ('YENISEI')
C
      return
      end
