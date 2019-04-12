      subroutine ZIZIA
     $(ALB,ALBK,XLM,XNE,HND,TE,CQT,CQA,N)
C
C     Rudolf Loeser, 1988 Dec 06
C---- Sets up ALB, the set of scattering albedo values used
C     with the KURUCZ opacities.
C     !DASH
      save
C     !DASH
      real*8 ALB, ALBK, CLM, CQA, CQM, CQT, HND, PNH, TE, XLM, XNE
      integer N, NCQ, NKA
      logical ALBZERO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(53),NCQ)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(101),CQM  )
      equivalence (RZQ(150),PNH  )
      equivalence (RZQ(151),CLM  )
C     !DASH
      external NAUGHTD, COMO, MOVE1, HI, BYE
C
C               ALB(N), ALBK(N), XNE(N), HND(N), CQT(NCQ), CQA(NCQ),
      dimension ALB(*), ALBK(*), XNE(*), HND(*), CQT(*),   CQA(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('ZIZIA')
C     !BEG
      call NAUGHTD   (ALB, 1, N, ALBZERO)
      if(ALBZERO) then
        if(NKA.le.0) then
          call COMO  (XLM, XNE, TE, HND, N, CQT, CQA, NCQ, CQM, PNH,
     $                CLM, ALB)
        else
          call MOVE1 (ALBK, N, ALB)
        end if
      end if
C     !END
      call BYE ('ZIZIA')
C
      return
      end
