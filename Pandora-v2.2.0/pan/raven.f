      subroutine RAVEN
     $(N,NL,NCK,NT,CHECK,TAU,S,RHO,RHOWT,XND,RK,BD,XNE,Z,HND,TDST,XNK,
     $ QHI,DMP,LINES)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Reads iterative summaries data.
C     (This is version 4 of RAVEN.)
C     !DASH
      save
C     !DASH
      real*8 BD, CHECK, HND, QHI, RHO, RHOWT, RK, S, TAU, TDST, XND,
     $       XNE, XNK, Z
      integer IN1, IN2, IT, M, N, NCK, NIT, NL, NT
      logical DMP
      character LINES*88
C     !COM
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
C     !DASH
C     !EJECT
      external LEYTE, INTRANS, HI, BYE
C
C               CHECK(N,NCK,NCKITR), TAU(N,NT,NTAITR), S(N,NT,NSSITR),
      dimension CHECK(N,NCK,*),      TAU(N,NT,*),      S(N,NT,*),
C
C               RHO(N,NT,NRHITR), RHOWT(N,NT,NRWITR), XND(N,NL,NNDITR),
     $          RHO(N,NT,*),      RHOWT(N,NT,*),      XND(N,NL,*),
C
C               RK(N,NRKITR), BD(N,NBKITR), XNE(N,NNEITR), Z(N,NZZITR),
     $          RK(N,*),      BD(N,*),      XNE(N,*),      Z(N,*),
C
C               QHI(N,NT,NQHITR), TDST(N,NTDITR), XNK(N,NNKITR),
     $          QHI(N,NT,*),      TDST(N,*),      XNK(N,*),
C
C               LINES(MXIBIS), HND(N,NHNITR)
     $          LINES(*),      HND(N,*)
C
      call HI ('RAVEN')
C     !BEG
      M = 0
  200 continue
C
        M = M+1
        if(M.gt.NIBIS) then
          goto 199
        end if
C
        NIT = IBNIT(M)
        IN1 = IBIN1(M)
        IN2 = IBIN2(M)
C
        if(DMP) then
          write (LINES(M),100) IBSRC(M),IBNAM(M),NIT,IBLEN(M),IN1,IN2,
     $                         IBITE(M),IBLIT(M),IBITH(M),IBIOV(M)
  100     format(A10,2X,A10,2X,8I8)
        end if
C     !EJECT
        goto (201, 202, 203, 204, 205, 206, 207, 208, 209,
     $        210, 211, 212, 213, 214) IBKOD(M)
  201   continue
          call INTRANS (IN1, IN2, 'RAVEN-1', IT)
          call LEYTE   (TAU(1,IT,NIT), IBLEN(M), IBADR(M))
          goto 200
  202   continue
          call LEYTE   (CHECK(1,(IN1-2),NIT), IBLEN(M), IBADR(M))
          goto 200
  203   continue
          call INTRANS (IN1, IN2, 'RAVEN-2', IT)
          call LEYTE   (RHO(1,IT,NIT), IBLEN(M), IBADR(M))
          goto 200
  204   continue
          call LEYTE   (RK(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  205   continue
          call LEYTE   (XND(1,IN1,NIT), IBLEN(M), IBADR(M))
          goto 200
  206   continue
          call INTRANS (IN1, IN2, 'RAVEN-3', IT)
          call LEYTE   (RHOWT(1,IT,NIT), IBLEN(M), IBADR(M))
          goto 200
  207   continue
          call LEYTE   (BD(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  208   continue
          call LEYTE   (XNE(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  209   continue
          call INTRANS (IN1, IN2, 'RAVEN-4', IT)
          call LEYTE   (QHI(1,IT,NIT), IBLEN(M), IBADR(M))
          goto 200
  210   continue
          call LEYTE   (Z(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  211   continue
          call INTRANS (IN1, IN2, 'RAVEN-5', IT)
          call LEYTE   (S(1,IT,NIT), IBLEN(M), IBADR(M))
          goto 200
  212   continue
          call LEYTE   (HND(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  213   continue
          call LEYTE   (TDST(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  214   continue
          call LEYTE   (XNK(1,NIT), IBLEN(M), IBADR(M))
          goto 200
  199 continue
C     !END
      call BYE ('RAVEN')
C
      return
      end
