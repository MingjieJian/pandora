      subroutine RORQUAL
     $(TER,CMCE,CACE,DENS,XNU,XNUC,P,AIJ,AATIJ,NPQ,LRQ,CEIJ)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Computes default values of collisional excitation coefficient.
C     (Note: DENS is only needed for Hydrogen.)
C
C     (See also CEREAL.)
C     (This is version 3 of RORQUAL.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, CACE, CEIJ, CEX, CMCE, DENS, P, TER, UNSET,
     $       XNU, XNUC
      integer I, IL, IU, IUL, LRQ, NL, NPQ, NTE
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(20),NTE)
      equivalence (JZQ( 2),NL )
C     !DASH
C     !EJECT
      external INDXUL, TYRONE, MESHED, MASHED,HI, BYE
C
C               TER(NTE), CACE(MUL), XNU(NSL), NPQ(NSL), CEIJ(NTE,MUL),
      dimension TER(*),   CACE(*),   XNU(*),   NPQ(*),   CEIJ(NTE,*),
C
C               AATIJ(NL,NL), P(NSL), CMCE(MUL), AIJ(NL,NL), XNUC(NSL),
     $          AATIJ(*),     P(*),   CMCE(*),   AIJ(*),     XNUC(*),
C
C               LRQ(NSL)
     $          LRQ(*)
C
      data UNSET,DUMP /-1.D0, .false./
C
      call HI ('RORQUAL')
C     !BEG
      if(DUMP) then
        call MESHED       ('RORQUAL', 2)
      end if
C
      do 102 IU = 2,NL
        do 101 IL = 1,(IU-1)
          call INDXUL     (IU, IL, IUL)
C
          do 100 I = 1,NTE
            if(CEIJ(I,IUL).eq.UNSET) then
              call TYRONE (IU, IL, AIJ, AATIJ, TER(I), DENS, XNU,
     $                     XNUC, P, NPQ, LRQ, DUMP, CEX)
              CEIJ(I,IUL) = CMCE(IUL)*CEX+CACE(IUL)
            end if
  100     continue
C
  101   continue
  102 continue
C
      if(DUMP) then
        call MASHED       ('RORQUAL')
      end if
C     !END
      call BYE ('RORQUAL')
C
      return
      end
