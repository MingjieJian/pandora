      subroutine CHINON
     $(J,NTE,TER,CII,TE,DENS,XNU,XNUC,NPQ,LRQ,NLE,CMCI,CACI,DUMP,CI)
C
C     Rudolf Loeser, 1994 May 09
C---- Retrieves/computes collisional ionization coefficient.
C     (See also BRUN.)
C     !DASH
      save
C     !DASH
      real*8 CACI, CI, CII, CIX, CMCI, DENS, RFAC, TE, TER, XNU, XNUC,
     $       ZERO
      integer J, LRQ, MCIOF, NLE, NPQ, NTE, jummy
      logical DUMP
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
      equivalence (RZQ( 42),RFAC )
      equivalence (KZQ(224),MCIOF)
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
      external LININT, ARMAGH, HI, BYE
C
C               CII(NTE,NSL), TER(NTE), CMCI(NSL), CACI(NSL), XNU(NSL),
      dimension CII(NTE,*),   TER(*),   CMCI(*),   CACI(*),   XNU(*),
C
C               NPQ(NSL), LRQ(NSL), NLE(NSL), XNUC(NSL)
     $          NPQ(*),   LRQ(*),   NLE(*),   XNUC(*)
C
      call HI ('CHINON')
C     !BEG
      if(RFAC.eq.ZERO) then
        CI = ZERO
      else
C
        if(MCIOF.gt.0) then
C----     Compute on-the-fly
          if(CMCI(J).eq.ZERO) then
            CI = CACI(J)
          else
            call ARMAGH (J, TE, DENS, XNU, XNUC, NPQ, LRQ, NLE, DUMP,
     $                   CIX)
            CI = CMCI(J)*CIX+CACI(J)
          end if
          CI = CI*RFAC
        else
C----     Use given tabular function of temperature (TER)
C         (The data table CII already incorporates RFAC)
          call LININT   (TER, 1, CII(1,J), 1, NTE, TE, CI, 1, 1, jummy)
        end if
C
      end if
C     !END
      call BYE ('CHINON')
C
      return
      end
