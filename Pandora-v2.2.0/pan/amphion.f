      subroutine AMPHION
     $(IU,IL,TE,AIJ,P,XNU,IONST,CE)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Pulls out the A of this transition, for NIOBE.
C     !DASH
      save
C     !DASH
      real*8 AIJ, CE, P, TE, XNU
      integer IL, IONST, IU, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
      external NIOBE, HI, BYE
C
C               AIJ(NL,NL), XNU(NSL), P(NSL)
      dimension AIJ(NL,*),  XNU(*),   P(*)
C
      call HI ('AMPHION')
C     !BEG
      call NIOBE  (IU, IL, TE, AIJ(IU,IL), P, XNU, IONST, CE)
C     !END
      call BYE ('AMPHION')
C
      return
      end
