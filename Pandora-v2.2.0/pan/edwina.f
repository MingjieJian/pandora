      subroutine EDWINA
     $(S,SN,N,LABEL,PRNT,IMG,FO,SO,RAT,RATE,SNE)
C
C     Rudolf Loeser, 1998 Oct 01.
C---- Edits Line Source Function.
C     (This is version 3 of EDWINA.)
C     !DASH
      save
C     !DASH
      real*8 FO, RAT, RATE, S, SN, SNE, SO, ZERO
      integer IMG, N, NERM, jummy1, jummy2
      logical PRNT, lummy
      character LABEL*25, qummy*1
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
      equivalence (KZQ( 95),NERM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, EDITH, ARRDIV, ARRMUL, NEWAID, HI, BYE
C
C               FO(N), SN(N), IMG(N), SO(N), RAT(N), RATE(N), SNE(N),
      dimension FO(*), SN(*), IMG(*), SO(*), RAT(*), RATE(*), SNE(*),
C
C               S(N)
     $          S(*)
C     !EJECT
C
      call HI ('EDWINA')
C     !BEG
C---- Duplicate computed sets
      call MOVE1  (S ,N,SO )
      call MOVE1  (SN,N,SNE)
C---- Edit SN
      call EDITH  (SNE ,N,ZERO,1,1,0,qummy,IMG,FO,jummy1,jummy2,lummy)
C---- Compute the ratio S/SN
      call ARRDIV (SO,SNE,RAT,N)
C---- Edit the ratio
      call MOVE1  (RAT,N,RATE)
      call EDITH  (RATE,N,ZERO,2,2,0,qummy,IMG,FO,jummy1,NERM  ,lummy)
C---- Compute product RATE*SNE = final S
      call ARRMUL (RATE,SNE,S,N)
C
C---- Dump or print message
      call NEWAID (PRNT,LABEL,N,IMG,SO,SN,SNE,RAT,RATE,S)
C     !END
      call BYE ('EDWINA')
C
      return
      end
