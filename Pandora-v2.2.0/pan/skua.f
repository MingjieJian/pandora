      subroutine SKUA
     $(N,Z,VSB,FXI,SN,YBRC,RHOST,RHOSO,DV,RV,H,G,GC,PD,F,ISB1,ISB2)
C
C     Rudolf Loeser, 1986 Jul 24
C---- Computes RHO by the Sobolev escape probability method.
C     !DASH
      save
C     !DASH
      real*8 ARG, DV, F, FXI, G, G1, G2, GC, GC1, GC2, H, ONE, PD, R1N,
     $       RAT, RHOSO, RHOST, RV, SBDMN, SBDMX, SBFEQ, SN, START, TEN,
     $       THREE, VEC, VSB, YBRC, Z, ZERO, ZSUM
      integer I, IQSFS, ISB1, ISB2, LIM, LU, N, NSUB
      logical DUMP, GOOD, SPHERE
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
      equivalence (RZQ( 92),SBFEQ)
      equivalence (RZQ( 93),SBDMX)
      equivalence (RZQ( 94),SBDMN)
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
      equivalence (IQQ( 31),IQSFS)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT(11),TEN   )
C
C---- SOBOLEV     as of 1993 Aug 20
      integer     MDSOB
      common      /SOBOLEV/ MDSOB
C     Counter value for Sobolev escape probability calculation.
C     .
C     !DASH
C     !EJECT
      external  PACA, ARGALI, MAGGOT, GULL, ADAIR, ERNE, ZERO1, DIVIDE,
     $          NATICA, ZAPUS, MOVE1, SENF, MASHED, HI, BYE
      intrinsic min
C
C               FXI(N), DV(N), RV(N), G(N), Z(N), VSB(N), SN(N), GC(N),
      dimension FXI(*), DV(*), RV(*), G(*), Z(*), VSB(*), SN(*), GC(*),
C
C               YBRC(N), RHOST(N), RHOSO(N), H(N), PD(N), F(N)
     $          YBRC(*), RHOST(*), RHOSO(*), H(*), PD(*), F(*)
C
      dimension VEC(4)
C
      call HI ('SKUA')
C     !BEG
C---- Get dump control
      call ARGALI    ('SKUA', DUMP)
C---- Set depth limit for computing integrals
      LIM = min(ISB2,N)
C---- Miscellaneous initialization
      SPHERE = IQSFS.gt.0
      call ZERO1     (G,     N)
      call ZERO1     (GC,    N)
      call ZERO1     (RHOSO, N)
C---- Compute DV and RV
      call MAGGOT    (N, Z, VSB, R1N, DV, RV, SPHERE)
      if(DUMP) then
        call GULL    (LIM, N, H, F, R1N, ISB1, ISB2, SBFEQ, SBDMX,
     $                SBDMN, Z, VSB, FXI, DV, RV)
      end if
C---- Compute G (by adaptive integration)
      do 100 I = 1,LIM
        if(SPHERE) then
          START = ZERO
          G1 = ZERO
        else
          call DIVIDE (FXI(I), (TEN*DV(I)), ARG)
          START = sqrt(ARG)
          call DIVIDE ((DV(I)*(START**3)), (THREE*FXI(I)), G1)
        end if
        VEC(1) = FXI(I)
        VEC(2) = DV(I)
        VEC(3) = RV(I)
        call SENF     (DUMP, 'ADAIR', I, LU)
        call ADAIR    (START, ONE, PACA, VEC, G2, SBFEQ, SBDMX,
     $                 SBDMN, LU, PD, MDSOB, NSUB, GOOD)
        if(LU.gt.0) then
          call MASHED ('ADAIR')
        end if
        if(DUMP) then
          call ERNE   (I, START, G1, G2, NSUB, GOOD, ' G',
     $                 'delta')
        end if
        G(I) = G1+G2
  100 continue
C     !EJECT
C---- Set up GC
      if(.not.SPHERE) then
        call MOVE1        (G, N, GC)
      else
C----   Compute GC (by adaptive integration)
        ZSUM = Z(N)+R1N
        do 101 I = 1,LIM
          call DIVIDE     ((R1N**2), ((ZSUM-Z(I))**2), RAT)
          if(RAT.lt.ONE) then
            START  = sqrt(ONE-RAT)
            GC1    = ZERO
            VEC(1) = FXI(I)
            VEC(2) = DV(I)
            VEC(3) = RV(I)
            call SENF     (DUMP, 'ADAIR', I, LU)
            call ADAIR    (START, ONE, PACA, VEC, GC2, SBFEQ, SBDMX,
     $                     SBDMN, LU, PD, MDSOB, NSUB, GOOD)
            if(LU.gt.0) then
              call MASHED ('ADAIR')
            end if
            if(DUMP) then
              call ERNE   (I, START, GC1, GC2, NSUB, GOOD, 'GC',
     $                     '    D')
            end if
          else
            GC2 = ZERO
          end if
          GC(I) = GC1+GC2
  101   continue
      end if
C---- Compute RHO
      call NATICA         (LIM, G, GC, YBRC, SN, RHOSO)
C
      if(DUMP) then
        call ZAPUS        (LIM, N, Z, VSB, G, GC, YBRC, SN, RHOSO,
     $                     RHOST, 'SKUA')
      end if
C     !END
      call BYE ('SKUA')
C
      return
      end
