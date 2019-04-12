      subroutine BOTTOM
     $(W,IW,N,DMP1,MSFT,CDW,DW,CNDT,EP,BS,SN,IMG,ANT,OMD,DLC,PA,PG,PB,
     $ PQ,PD,T,FNDT,DEL,UNT,IDDL,BF,BA,S,EDITS,XJBAR,RHO,MEDUSA)
C
C     Rudolf Loeser, 2004 May 07
C
C---- Computes: S (and EDITS), XJBAR, RHO and MEDUSA,
C     (and also: T, UNT, DEL, IDDL, FNDT, BA and BF), given the
C     frequency/angle integrals: PA, PG, PB, PQ, PD, ANT, OMD, and DLC.
C
C     (This is version 5 of BOTTOM.)
C     !DASH
      save
C     !DASH
      real*8 ANT, BA, BF, BS, CDW, CNDT, DEL, DLC, DW, EP, FNDT, OMD,
     $       PA, PB, PD, PG, PQ, RHO, S, SN, T, UNT, W, XJBAR
      integer ICP, IDDL, IEM, IFO, IGA, IMG, IN, IS, IVEC, IW, JJ, KK,
     $        MEDUSA, MOX, MSFT, N
      logical BOTH, DMP1, EDITS
C     !DASH
      external NODDY, BRONZE, LINDEN, KATSURA, BRASS, MESHED, MASHED,
     $         RODA, KOBE, PAPA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DW(N), CNDT(N), EP(N), BS(N), XJBAR(N), IMG(N), ANT(N),
      dimension DW(*), CNDT(*), EP(*), BS(*), XJBAR(*), IMG(*), ANT(*),
C
C               OMD(N), DLC(N), PA(N,N), PG(N,N), PB(N), PD(N), UNT(N),
     $          OMD(*), DLC(*), PA(*),   PG(*),   PB(*), PD(*), UNT(*),
C
C               FNDT(N), DEL(N), PQ(N), BF(N), BA(N), RHO(N), SN(N),
     $          FNDT(*), DEL(*), PQ(*), BF(*), BA(*), RHO(*), SN(*),
C
C               S(N), T(N)
     $          S(*), T(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IEM   ),(IN( 2),IFO   ),(IN( 3),IGA   ),(IN( 4),ICP   ),
     $(IN( 5),IVEC  )
C     !EJECT
C
      call HI ('BOTTOM')
C     !BEG
C     (Get, and allocate, W allotment)
      call NODDY     (IN, IS, MOX, 'BOTTOM')
C
      if(DMP1) then
        call MESHED  ('BOTTOM', 2)
      end if
C---- Compute tables of T, FNDT, DEL and UNT, and index IDDL
      call RODA      (N, DMP1, CDW, DW, CNDT, ANT, OMD, PD, UNT, T,
     $                FNDT, DEL, IDDL)
C
C---- Establish limiting indices of combined solution
C     JJ is the index of the end of "full"
C     KK is the index of the start of "direct"
      call PAPA      (MSFT, IDDL, N, BOTH, JJ, KK)
      if(MSFT.eq.0) then
C----   Obtain S from matrix equation --- "full" solution ( Edit? )
        call LINDEN  (W, IW, N, JJ, DMP1, EP, BS, PA, PB, DEL, DLC,
     $                FNDT, BF, BA, W(IEM), SN, IMG, S, EDITS, MEDUSA)
      end if
      if((MSFT.eq.1).or.BOTH) then
C----   Obtain S iteratively from S(n) --- "direct" solution ( Edit ?)
        call KATSURA (W, IW, KK, N, DMP1, SN, PA, PG, OMD, DLC, FNDT,
     $                EP, BS, IMG, W(IFO), W(IGA), W(ICP), W(IVEC), S)
      end if
C---- Final housekeeping for "direct" solution
      call KOBE      (MSFT, BOTH, N, IDDL, KK, EDITS, BA, BF)
C
C---- Compute JBAR (and CP for RHO) ( Edit! )
      call BRASS     (1, N, S, PA, PG, OMD, DLC, FNDT, W(IGA), W(ICP),
     $                IMG, W(IFO), DMP1, XJBAR)
C---- Compute RHO ( Edit? )
      call BRONZE    (N, PQ, W(ICP), S, IMG, W(IFO), DMP1, RHO)
C
      if(DMP1) then
        call MASHED  ('BOTTOM')
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'BOTTOM')
C     !END
      call BYE ('BOTTOM')
C
      return
      end
