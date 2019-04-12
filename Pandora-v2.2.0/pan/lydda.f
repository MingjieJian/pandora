      subroutine LYDDA
     $(CQT,CQA,NCQ,TE, CQM, CQV)
C
C     Rudolf Loeser, 1995 May 31
C---- Computes CQV, for COMO.
C     !DASH
      save
C     !DASH
      real*8 CQA, CQM, CQT, CQV, TE, ZERO
      integer NCQ, jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LININT, HI, BYE
C
C               CQT(NCQ), CQA(NCQ)
      dimension CQT(*),   CQA(*)
C
      call HI ('LYDDA')
C     !BEG
      if(CQM.gt.ZERO) then
        CQV = CQM
      else
        call LININT (CQT, 1, CQA, 1, NCQ, TE, CQV, 4, 1, jummy)
      end if
C     !END
      call BYE ('LYDDA')
C
      return
      end
