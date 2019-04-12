      subroutine CEREAL
     $(I,J,TER,CEI,TE,DENS,AIJ,AATIJ,XNU,XNUC,P,NPQ,LRQ,CMCE,CACE,
     $ DUMP,CE)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Retrieves/computes collisional de-excitation coefficient.
C     (See also RORQUAL.)
C     !DASH
      save
C     !DASH
      real*8 A, AATIJ, AIJ, CACE, CE, CEI, CEIJ, CEX, CMCE, DENS, P,
     $       RFAC, TE, TER, XNU, XNUC, ZERO
      integer I, IL, IU, IUL, J, LRQ, MCEOF, NPQ, NTE, jummy
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
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
      equivalence (RZQ( 42),RFAC )
      equivalence (KZQ(223),MCEOF)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, INDXUL, LININT, TYRONE, HI, BYE
C
C               CEI(NTE,MUL), TER(NTE), CMCE(MUL), CACE(MUL), XNU(NSL),
      dimension CEI(NTE,*),   TER(*),   CMCE(*),   CACE(*),   XNU(*),
C
C               XNUC(NSL), LRQ(NSL), P(NSL), AIJ(NL,NL), AATIJ(NL,NL),
     $          XNUC(*),   LRQ(*),   P(*),   AIJ(*),     AATIJ(*),
C
C               NPQ(NSL)
     $          NPQ(*)
C
      call HI ('CEREAL')
C     !BEG
      if(RFAC.eq.ZERO) then
        CE = ZERO
      else
C----   Set up transition indices
        if(I.gt.J) then
          IU = I
          IL = J
        else if(J.gt.I) then
          IU = J
          IL = I
        else
          write (MSSLIN(1),100) I,J
  100     format('Transition indices for CE: I =',I12,', J =',I12,
     $           ', do not make sense.')
          call HALT     ('CEREAL', 1)
        end if
        call INDXUL     (IU, IL, IUL)
C
        if(MCEOF.gt.0) then
C----     Compute on-the-fly
          if(CMCE(IUL).eq.ZERO) then
            CE = CACE(IUL)
          else
            call TYRONE (IU, IL, AIJ, AATIJ, TE, DENS, XNU, XNUC, P,
     $                   NPQ, LRQ, DUMP, CEX)
            CE = CMCE(IUL)*CEX+CACE(IUL)
          end if
          CE = CE*RFAC
        else
C----     Use tabular function of temperature (TER)
C         (The data table CEI already incorporates RFAC)
          call LININT   (TER, 1, CEI(1,IUL), 1, NTE, TE, CE, 1, 1,
     $                   jummy)
        end if
C
      end if
C     !END
      call BYE ('CEREAL')
C
      return
      end
