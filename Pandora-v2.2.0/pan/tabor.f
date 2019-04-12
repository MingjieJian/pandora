      subroutine TABOR
     $(IU,IL,XNE,TE,V,IST,DW,FRCDL,FMCDL,FSTKM,MAX,DUMP,LDL,DWN,CDL,
     $ GOOD)
C
C     Rudolf Loeser, 1992 Jan 21
C---- Computes a table of major components of the Hydrogen (u/l)-line
C     produced by Stark splitting.
C     (This is version 3 of TABOR.)
C     !DASH
      save
C     !DASH
      real*8 CCON, CDL, CDR, CHLF, CRAW, CRIT, DCON, DDR, DELTA, DHLF,
     $       DMAX, DMIN, DRAW, DRR, DW, DWN, FAC, FC, FD, FMCDL, FRAC,
     $       FRCDL, FSTKM, ONE, SUM, TE, TEN, V, WORK, XNE, ZERO, ZRAW
      integer IL, IPNT, IST, ITER, IU, K, LDL, LIM, M, MAX, MP, N, NL,
     $        NS
      logical DUMP, GOOD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external  GRADER, GERARD, TIMOTHY, SORT, ORDERD, TYBALT, BERNARD,
     $          BINGE, UMBERTO, ARRSUM, GIFFORD, CONDIV, MOVE1, CICADA,
     $          ADOLF, DUSA, DACE, MASHED, HI, BYE
      intrinsic min
C
C               DWN(MAX), CDL(MAX)
      dimension DWN(*),   CDL(*)
C
      parameter   (NS = 6)
      parameter   (NL = 15)
      parameter   (LIM = (NL*(NL-1)/2)**2)
C
      dimension   DRAW(LIM), CRAW(LIM), ZRAW(LIM), IPNT(LIM), DDR(LIM),
     $            DCON(LIM), CCON(LIM), DHLF(LIM/2+1), CHLF(LIM/2+1),
     $            CDR(LIM), FD(NS), FC(NS), WORK(LIM)
C
      data FD /2.D0, 3.D0, 5.D0, 6.D0, 8.D0, 1.D1/
      data FC /2.D0, 4.D0, 1.D1, 2.D1, 3.5D1, 5.D1/
      data FAC,DELTA /1.414213562373095D0, 1.D-12/
C
      call HI ('TABOR')
C     !BEG
      if(DUMP) then
        call GRADER (IU, IL, XNE, TE, V, DW, FRCDL, FMCDL, FSTKM,
     $               'TABOR')
      end if
C
      DMIN = DW/TEN
      DMAX = DW*TEN
C
      CRIT = DMIN
      FRAC = FRCDL
      ITER = 0
      GOOD = .true.
      K = 0
C     !EJECT
C---- Get complete set of raw tables, and sort: DRAW, CRAW, M
      call TIMOTHY     (DUMP, IU, IL, XNE, FSTKM, LIM, M,
     $                  DRAW, CRAW, ZRAW)
      call SORT        (DRAW, M, IPNT, 'Raw Ds')
      call ORDERD      (CRAW, IPNT, M, WORK)
      call ORDERD      (ZRAW, IPNT, M, WORK)
      if(DUMP) then
        call TYBALT    (IU, IL, M, ZRAW, DRAW, CRAW)
      end if
C---- Consolidate duplicates: DCON, CCON, MP
      call BINGE       (DRAW, CRAW, M, DELTA, DCON, CCON, MP)
      if(DUMP) then
        call GERARD    (IU, IL, MP, DCON, CCON)
      end if
C
      if(DCON(MP).lt.DMIN) then
C----   Components are not separated significantly
        DWN(1) = ZERO
        CDL(1) = ONE
        LDL = 1
        DRR = ZERO
        K = 1
        N = 1
        go to 103
      end if
C
C---- Truncate far wings: DCON, CCON, N
      call ADOLF       (DCON, CCON, MP, DMAX, N)
      if(DUMP) then
        call GIFFORD   (IU, IL, N, DCON, CCON, DMAX)
      end if
C     !EJECT
      GOOD = N.le.MAX
      if(GOOD) then
C----   Accept this truncated theoretical set (but eliminate junk)
        call DUSA      (DCON, CCON, N, FRCDL, DELTA, DHLF, CHLF)
        if(DUMP) then
          call UMBERTO (IU, IL, N, DCON, CCON, FRCDL, ITER)
        end if
        call MOVE1     (DCON, N, DWN)
        call MOVE1     (CCON, N, CDL)
        LDL = N
        DRR = DCON(N)
        K = N
        go to 102
      end if
C
  101 continue
        ITER = ITER+1
C----   Coalesce "unresolvable" components: DDR, CDR, K
        call DACE      (DCON, CCON, N, CRIT, DHLF, CHLF, DDR, CDR, K)
        if(DUMP) then
          call BERNARD (IU, IL, K, DDR, CDR, CRIT, ITER)
        end if
C----   Delete "minor" lines (if any): DDR, CDR, K
        call DUSA      (DDR, CDR, K, FRAC, DELTA, DHLF, CHLF)
        if(DUMP) then
          call UMBERTO (IU, IL, K, DDR, CDR, FRAC, ITER)
        end if
        if(K.gt.MAX) then
C----     Try again with relaxed criteria
          if(ITER.le.NS) then
            CRIT = DMIN*FD(ITER)
            FRAC = FRCDL*FC(ITER)
            FRAC = min(FRAC,FMCDL)
          else
            CRIT = CRIT*FAC
          end if
          go to 101
        end if
C     !EJECT
      GOOD = K.le.MAX
      if(GOOD) then
C----   Accept this set
        LDL = K
        call MOVE1     (DDR, LDL, DWN)
        call MOVE1     (CDR, LDL, CDL)
        DRR = DDR(K)
      end if
C
  102 continue
      if(GOOD) then
C       Normalize the weights
        call ARRSUM    (CDL, K, SUM)
        call CONDIV    (SUM, CDL, K)
      end if
C
  103 continue
C
      if(DUMP) then
        call MASHED    ('TABOR')
      end if
C
C---- Print summary
      call CICADA      (IU, IL, IST, XNE, FSTKM, TE, V, DW, FRCDL,
     $                  FMCDL, CRIT, FRAC, ITER, M, MP, N, K, DRAW(M),
     $                  DRR, MAX, GOOD)
C     !END
      call BYE ('TABOR')
C
      return
      end
