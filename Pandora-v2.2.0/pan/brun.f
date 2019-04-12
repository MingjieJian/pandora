      subroutine BRUN
     $(TER,CMCI,CACI,DENS,XNU,XNUC,NPQ,LRQ,NLE,CII)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Computes default values of collisional ionization coefficients.
C     (Note: DENS is only needed for Hydrogen.)
C
C     (See also CHINON.)
C     (This is version 3 of BRUN.)
C     !DASH
      save
C     !DASH
      real*8 CACI, CII, CIX, CMCI, DENS, TER, UNSET, XNU, XNUC
      integer I, IONST, J, LRQ, NLE, NPQ, NSL, NTE
      logical DMPS, DUMP, HYDR
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(20),NTE)
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
C     !DASH
C     !EJECT
      external SULTANA, ARMAGH, MESHED, MASHED, HI, BYE
C
C               CII(NTE,NSL), CACI(NSL), CMCI(NSL), TER(NTE), XNU(NSL),
      dimension CII(NTE,*),   CACI(*),   CMCI(*),   TER(*),   XNU(*),
C
C               NPQ(NSL), LRQ(NSL), NLE(NSL), XNUC(NSL)
     $          NPQ(*),   LRQ(*),   NLE(*),   XNUC(*)
C
      data UNSET,DUMP,DMPS /-1.D0, .false., .true./
C
      call HI ('BRUN')
C     !BEG
      HYDR = QELSM(:3).eq.'H  '
      call SULTANA      (HYDR, IONST, DMPS)
C
      if(DUMP) then
        call MESHED     ('BRUN', 2)
      end if
C
      do 101 J = 1,NSL
        do 100 I = 1,NTE
          if(CII(I,J).eq.UNSET) then
            call ARMAGH (J, TER(I), DENS, XNU, XNUC, NPQ, LRQ, NLE,
     $                   DUMP, CIX)
            CII(I,J) = CMCI(J)*CIX+CACI(J)
          end if
  100   continue
  101 continue
C
      if(DUMP) then
        call MASHED     ('BRUN')
      end if
C     !END
      call BYE ('BRUN')
C
      return
      end
