      subroutine KILOSA
     $(N,NL,MCXK,LCX,NPQ,LRQ,TE,CXX,CXXP,CCHX,ICXDP,G,LISTXL,ARR,Z,ZL,
     $ LUP,DUMP,WRK)
C
C     Rudolf Loeser, 1990 Nov 27
C---- Computes the upper-level charge exchange terms CXX and CXXP.
C     !DASH
      save
C     !DASH
      real*8 ARR, CCHX, CXX, CXXP, G, TE, TIME, TIN, TOUT, WRK, Z, ZL
      integer ICXDP, J, LCX, LISTXL, LRQ, LUP, MCXK, N, NL, NNL, NPQ
      logical DUMP
C     !DASH
      external CHECKER, COLOBA, HUILA, HILLAH, LINER, SECOND, MASHED,
     $         NAMMU, ZERO1, DYLE, HI, BYE
C
C               NPQ(NL), LRQ(NL), CXXP(N,NL), TE(N), LISTXL(NL), ZL(N),
      dimension NPQ(*),  LRQ(*),  CXXP(N,*),  TE(*), LISTXL(*),  ZL(*),
C
C               LCX(NL), ARR(N,NL), Z(N), CXX(N,NL), WRK(NPQLM,NPQLM),
     $          LCX(*),  ARR(N,*),  Z(*), CXX(N,*),  WRK(*),
C
C               G(N,NL)
     $          G(N,*)
C     !EJECT
C
      call HI ('KILOSA')
C     !BEG
      call SECOND     (TIN)
C
      NNL = N*NL
      call ZERO1      (G,    NNL)
      call ZERO1      (CXX,  NNL)
      call ZERO1      (CXXP, NNL)
C
C---- Compute DELTA
      call DYLE       (MCXK)
C
      if(DUMP) then
        call HUILA    (MCXK, CCHX, WRK, ICXDP, 'KILOSA')
      end if
      do 100 J = 1,NL
        if(LCX(J).gt.0) then
          call COLOBA (N, NPQ(J), LRQ(J), CXX(1,J), CXXP(1,J), G(1,J),
     $                 TE, CCHX, ICXDP, J, DUMP)
        end if
  100 continue
      if(DUMP) then
        call MASHED   ('KILOSA')
      end if
C
C---- Print [ ? ]
      call HILLAH     (N, NL, LUP, CXX, CXXP, G)
C---- Plot [ ? ]
      call NAMMU      (N, NL, LUP, LCX, CXX, CXXP, G, LISTXL, ARR,
     $                 Z, ZL)
C---- Checksums
      call CHECKER    (G,    1, NNL, 'G  for CHXC')
      call CHECKER    (CXXP, 1, NNL, 'XP for CHXC')
      call CHECKER    (CXX,  1, NNL, 'X  for CHXC')
C
      if(LUP.gt.0) then
        call SECOND   (TOUT)
        TIME = TOUT-TIN
        call LINER    (2, LUP)
        write (LUP,101) TIME
  101   format(' ','Time',F9.1,' sec.')
      end if
C     !END
      call BYE ('KILOSA')
C
      return
      end
