      subroutine DIOCLES
     $(IADRS,XCBL,ICE,K,DL,PHI,GTN,XQSF,VXI,XRD,YRD,S)
C
C     Rudolf Loeser, 1982 Jan 26
C---- Updates Continuum Blocks for SHEBA.
C     !DASH
      save
C     !DASH
      real*8 DL, GTN, PHI, S, VXI, XCBL, XQSF, XRD, YRD
      integer IADRS, ICE, J, K, KKDL, KKZABS, KKZAXA, KKZAYA, KKZBDN,
     $        KKZBNM, KKZSCA, KKZSCR, LU, N, jummy1, jummy2
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(35),KKZABS)
      equivalence (KKK(36),KKZSCA)
      equivalence (KKK(37),KKZSCR)
      equivalence (KKK(41),KKZBNM)
      equivalence (KKK(57),KKZBDN)
      equivalence (KKK(58),KKZAXA)
      equivalence (KKK(59),KKZAYA)
      equivalence (KKK(46),KKDL  )
C     !DASH
C     !EJECT
      external BOOM, LEYTE, BOHOL, EGG, HI, BYE
C
C               IADRS(K), VXI(N,K), PHI(N,K), XQSF(N,K), DL(K), GTN(N),
      dimension IADRS(*), VXI(N,*), PHI(N,*), XQSF(N,*), DL(*), GTN(*),
C
C               XRD(N,K), YRD(N,K), S(N), XCBL(Miklen)
     $          XRD(N,*), YRD(N,*), S(*), XCBL(*)
C
      call HI ('DIOCLES')
C     !BEG
      call BOOM    (LU, jummy1, jummy2)
      KILROY = .true.
      do 100 J = 1,K
        call LEYTE (XCBL, MIKLEN, IADRS(J))
        call EGG   (KILROY, N, LU, K, J, ICE, DL, PHI(1,J), GTN,
     $              S, XQSF(1,J), VXI(1,J), XRD(1,J), YRD(1,J),
     $              XCBL(KKDL),   XCBL(KKZABS), XCBL(KKZSCA),
     $              XCBL(KKZSCR), XCBL(KKZBNM), XCBL(KKZBDN),
     $              XCBL(KKZAXA), XCBL(KKZAYA))
        call BOHOL (XCBL, MIKLEN, IADRS(J))
  100 continue
C     !END
      call BYE ('DIOCLES')
C
      return
      end
