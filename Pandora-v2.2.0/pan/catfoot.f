      subroutine CATFOOT
     $(INDX,XLM,N,NOPAC,XNE,HN,TE,BDHM,CONT)
C
C     Rudolf Loeser, 1988 Oct 26
C---- Computes a set of H- bound-free opacity values.
C     (This is version 2 of CATFOOT.)
C     !DASH
      save
C     !DASH
      real*8 A, B, BDHM, C1, C2, C3, C4, CONT, EX, F, F3, FF, HN, TE,
     $       XLM, XLMAX, XNE, ZERO
      integer I, INDX, J, N, NOPAC
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external CATCLAW, ZEROD, HI, BYE
C
C               XNE(N), TE(N), BDHM(N), HN(N,Limp), CONT(Nopac,N)
      dimension XNE(*), TE(*), BDHM(*), HN(*),      CONT(*)
C
      data XLMAX /1.6421D4/
      data C1, C2, C3, C4 /5.D3, 2.92D-39, 8.756D3, 2.88D4/
C
      call HI ('CATFOOT')
C     !BEG
      call CATCLAW (XLM, A)
      if((XLM.gt.ZERO).and.(XLM.lt.XLMAX).and.(A.ne.ZERO)) then
        J = INDX-NOPAC
        do 100 I = 1,N
          J  = J+NOPAC
          F  = C1/TE(I)
          FF = sqrt(F)
          F3 = FF**3
          EX = exp(C3/TE(I))
          B  = C2*XNE(I)*HN(I)*F3*EX
          EX = exp(-(C4/XLM)*F)
C
          CONT(J) = A*B*(BDHM(I)-EX)
  100   continue
C
      else
        call ZEROD (CONT(INDX), NOPAC, N)
      end if
C     !END
      call BYE ('CATFOOT')
C
      return
      end
