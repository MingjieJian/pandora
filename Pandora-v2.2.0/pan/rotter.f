      subroutine ROTTER
     $(INDEX,N,TE,PF,CFHE)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Computes CFHE (Helium) for lower-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, CFHE, CSP, DNU, EXT, FAC, HALF, PF, TE, XNK,
     $       XNUKHE, ZERO, dummy
      integer I, INDEX, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external RASH, PROD, ZERO1, HI, BYE
C
C               TE(N), PF(N), CFHE(N)
      dimension TE(*), PF(*), CFHE(*)
C
      data XNUKHE /5.9485D0/
C
      call HI ('ROTTER')
C     !BEG
      call RASH     (2, INDEX, A, dummy, dummy, dummy, CSP, XNK)
      if(A.ne.ZERO) then
C
        DNU = XNK-XNUKHE
        FAC = HALF/CSP
        do 100 I = 1,N
          call PROD (TE(I), DNU, 1, ARG, EXT)
          CFHE(I) = (FAC*PF(I))*EXT
  100   continue
C
      else
        call ZERO1  (CFHE, N)
      end if
C     !END
      call BYE ('ROTTER')
C
      return
      end
