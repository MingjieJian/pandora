      subroutine FOSTUR
     $(N,LDL,LINLDL,JDDL,IHSSP,XNU,XNE,TE,V,ISTRK,FRCDL,FMCDL,FSTKM,
     $ IU,IL,LDLMX,IST,STE,DWN,CDL,DDL,GOOD)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Computes and/or checks blended line components for Hydrogen.
C     (This is version 2 of FOSTUR.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DWEST, DWN, FMCDL, FRCDL, FSTKM, STE, TE, V, XNE,
     $       XNEST, XNU
      integer IHSSP, IL, IST, ISTRK, IU, JDDL, JST, LDL, LDLMX, LINLDL,
     $        N, NC
      logical GOOD
C     !DASH
      external  CHALK, DAWN, EDILE, DATINI, HI, BYE
C
C               XNE(N), DWN(LDLMX), CDL(LDLMX), TE(N), V(N), DDL(LDLMX),
      dimension XNE(*), DWN(*),     CDL(*),     TE(*), V(*), DDL(*),
C
C               XNU(NSL)
     $          XNU(*)
C
      call HI ('FOSTUR')
C     !BEG
      GOOD = .true.
C
      if((IHSSP.gt.0).and.(LDL.le.1)) then
        call CHALK  (XNE, N, ISTRK, IST, STE, JST, XNEST)
        call DAWN   (IU, IL, XNU, JST, TE, V, DWEST)
C
        call EDILE  (IU, IL, LDLMX, XNEST, TE(JST), V(JST), JST, DWEST,
     $               FRCDL, FMCDL, FSTKM, NC, DWN, CDL, GOOD)
C
        if((NC.gt.1).and.(LDL.ne.NC).and.GOOD) then
          LDL    = NC
          LINLDL = LDL
          JDDL   = JDDL+1
        end if
      end if
C
      if(GOOD) then
        call DATINI (IU, IL, XNU, LDL, DDL, DWN)
      end if
C     !END
      call BYE ('FOSTUR')
C
      return
      end
