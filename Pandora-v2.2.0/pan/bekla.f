      subroutine BEKLA
     $(HYDR,MESS,IU,IL,TE,AIJ,P,XNU,XNUC,NPQ,LRQ,CE,OK)
C
C     Rudolf Loeser, 2006 Mar 30
C---- Drives DOMINO to compute CE.
C     (This is version 3 of BEKLA.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CE, CEDMN, CEDMX, CEFEQ, P, TE, XNU, XNUC, ZERO
      integer IL, IU, LRQ, NL, NPQ, NSUB
      logical GOOD, HYDR, MESS, OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
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
      equivalence (RZQ(127),CEFEQ)
      equivalence (RZQ(126),CEDMX)
      equivalence (RZQ(125),CEDMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external DOMINO, HI, BYE
C
C               AIJ(NL,NL), XNUC(NSL), XNU(NSL), NPQ(NSL), LRQ(NSL),
      dimension AIJ(NL,*),  XNUC(*),   XNU(*),   NPQ(*),   LRQ(*),
C
C               P(NSL)
     $          P(*)
C
      call HI ('BEKLA')
C     !BEG
      OK = AIJ(IU,IL).ne.ZERO
      if(OK) then
        call DOMINO (IU, IL, TE, AIJ(IU,IL), HYDR, MESS, XNU, XNUC, P,
     $               LRQ, NPQ, CEFEQ, CEDMX, CEDMN, CE, NSUB, GOOD, OK)
      else
        CE = ZERO
      end if
C     !END
      call BYE ('BEKLA')
C
      return
      end
