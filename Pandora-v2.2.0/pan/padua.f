      subroutine PADUA
     $(EDIT,ITUS,CWU,KNW,SKIP,LIMIT,CWS,ITS)
C
C     Rudolf Loeser, 1996 Apr 22
C---- Adjusts CWU for MILAN, if necessary and possible.
C     !DASH
      save
C     !DASH
      real*8 CWS, CWU, EDIT, ONE, TWO, ZERO
      integer ITS, ITUS, KNW, LIMIT
      logical SKIP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               CWS(LIMIT), ITS(LIMIT)
      dimension CWS(*),     ITS(*)
C
      call HI ('PADUA')
C     !BEG
      SKIP = .false.
C
      if((EDIT.eq.(-ONE)).and.(ITUS.gt.1)) then
        if(KNW.lt.LIMIT) then
          CWU = CWU/TWO
          KNW = KNW+1
          CWS(KNW) = CWU
          ITS(KNW) = ITUS
          SKIP = .true.
          EDIT = ZERO
        end if
      end if
C     !END
      call BYE ('PADUA')
C
      return
      end
