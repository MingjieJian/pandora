      subroutine DATINI
     $(IU,IL,XNU,LDL,DDL,DWN)
C
C     Rudolf Loeser, 1991 Jul 25
C---- Checks input values of DDL and DWN.
C     !DASH
      save
C     !DASH
      real*8 DDL, DWN, W, WVN, X, XLM, XNU, ZERO
      integer I, IL, IU, LDL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ANGIE, WANDA, HI, BYE
C
C               DDL(LDL), DWN(LDL), XNU(NSL)
      dimension DDL(*),   DWN(*),   XNU(*)
C
      call HI ('DATINI')
C     !BEG
      call ANGIE     ((XNU(IU)-XNU(IL)),XLM)
      call WANDA     (XLM,WVN)
C
C---- Make sure that DDL(i) .ne. 0 when DWN(i) .ne. 0
      do 100 I = 1,LDL
        if((DDL(I).eq.ZERO).and.(DWN(I).ne.ZERO)) then
          call WANDA ((WVN+DWN(I)),X)
          DDL(I) = X-XLM
        end if
  100 continue
C
C---- Make sure DWN(i) .ne. 0 when DDL(i) .ne. 0
      do 101 I = 1,LDL
        if((DWN(I).eq.ZERO).and.(DDL(I).ne.ZERO)) then
          call WANDA ((XLM+DDL(I)),W)
          DWN(I) = W-WVN
        end if
  101 continue
C     !END
      call BYE ('DATINI')
C
      return
      end
