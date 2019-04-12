      subroutine MULIAK
     $(BDN,BDD,BDI,N,NL,ZION,WT)
C
C     Rudolf Loeser, 1989 Dec 11
C---- Computes weighted BDI (with optional dump).
C     !DASH
      save
C     !DASH
      real*8 BDD, BDI, BDN, ONE, W, WBDIR, WT, Z, ZERO, ZION
      integer I, J, KB1WA, KB1WB, KB1WS, KLOG, N, NL
      logical PRNT
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
      equivalence (KZQ(167),KB1WA)
      equivalence (KZQ(168),KB1WB)
      equivalence (RZQ(109),WBDIR)
      equivalence (KZQ(179),KB1WS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external YINNOP, UMILKA, WEIGHT, ZIRCON, WAIN, HI, BYE
C
C               BDI(N,NL), BDD(N,NL), BDN(N,NL), ZION(N), WT(N)
      dimension BDI(N,*),  BDD(N,*),  BDN(N,*),  ZION(*), WT(*)
C
      data KLOG  /1/
C
      call HI ('MULIAK')
C     !BEG
C---- Set up printout controls (? print header)
      call YINNOP       (KB1WS, KB1WA, KB1WB, WBDIR, PRNT)
C
C---- Set up table of weights
      call UMILKA       (ZION, WT, N)
C---- Loop over all levels
      do 101 J = 1,NL
C
        do 100 I = 1,N
          W = WBDIR*WT(I)
          call WEIGHT   (BDN(I,J), BDD(I,J), W, KLOG, Z)
          if(Z.eq.ZERO) then
C====
C           Intervene !!!  (2005 Jul 21)
            if(W.eq.ZERO) then
              Z = BDD(I,J)
            else
              Z = BDN(I,J)
            end if
C====
          end if
          if((J.eq.1).and.PRNT) then
            call ZIRCON (I, BDN(I,J), BDD(I,J), W, Z)
          end if
          BDI(I,J) = Z
  100   continue
  101 continue
C
      if(PRNT) then
        call WAIN
      end if
C     !END
      call BYE ('MULIAK')
C
      return
      end
