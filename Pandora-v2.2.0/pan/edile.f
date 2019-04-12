      subroutine EDILE
     $(IU,IL,LDLMX,XNEST,TEST,VST,JST,DWEST,FRCDL,FMCDL,FSTKM,
     $ LDL,DWN,CDL,GOOD)
C
C     Rudolf Loeser, 1992 Jan 29
C---- Sets up blended-line components resulting from Stark splitting,
C     for Hydrogen.
C     (This is version 2 of EDILE.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DWEST, DWN, FMCDL, FRCDL, FSTKM, TEST, VST, XNE,
     $       XNEST, ZERO
      integer IL, IU, JST, LDL, LDLMX
      logical DUMP, GOOD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external TABOR, HI, BYE
C
C               DWN(LDLMX), CDL(LDLMX)
      dimension DWN(*),     CDL(*)
C
      call HI ('EDILE')
C     !BEG
      if((LDLMX.gt.1).and.(XNEST.ne.ZERO)) then
        XNE  = XNEST
        DUMP = .false.
        if(XNE.lt.ZERO) then
          XNE  = -XNE
          DUMP = .true.
        end if
C
        call TABOR (IU, IL, XNE, TEST, VST, JST, DWEST, FRCDL, FMCDL,
     $              FSTKM, LDLMX, DUMP, LDL, DWN, CDL, GOOD)
C
      else
        LDL  = 1
        GOOD = .true.
      end if
C     !END
      call BYE ('EDILE')
C
      return
      end
