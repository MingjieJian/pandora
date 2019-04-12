      subroutine TETUAN
     $(IU,IL,K,IONST,LU,TE,CE)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Computes CE(u,l) for selected transitions of Carbon I - V
C     (as indicated by 1 .le. IONST .le. 5).
C     Returns CE = 0 if no data are available.
C     If LU > 0, writes dump printout to unit LU.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CE, D, E, E1Y, F, FAC, GAMMA, PL, RT, TE, XNUL,
     $       XNUU, Y, ZERO
      integer IL, IONST, IU, K, KEQ, LU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external CEUTA, MELILLA, HUNK, DIVIDE, HI, BYE
C
      data FAC /8.629D-6/
C
      call HI ('TETUAN')
C     !BEG
      CE = ZERO
C
      call CEUTA     (K, IU, IL, IONST, KEQ, XNUU, XNUL, PL,
     $                A, B, C, D, E, F)
      if(KEQ.gt.0) then
        call HUNK    (TE, (XNUU-XNUL), 1, Y)
        call MELILLA (KEQ, Y, A, B, C, D, E, F, E1Y, GAMMA)
        RT = sqrt(TE)
        call DIVIDE  ((FAC*GAMMA), (PL*RT), CE)
C
        if(LU.gt.0) then
          write (LU,100) IONST,IU,IL,KEQ,XNUU,XNUL,PL
  100     format(' ','Carbon-',I1,' transition (',I2,'/',I2,');',
     $               ' eq# =',I3,', nu(u) =',F9.5,', nu(l) =',F9.5,
     $               ', g(l) =',F9.5)
          write (LU,101) A,B,C,D,E,F
  101     format(' ','A,B,C,D,E,F',1P6E15.6)
          write (LU,102) TE,Y,E1Y,GAMMA,CE
  102     format(' ','TE =',1PE12.4,', y =',E14.6,', E1(y) =',E14.6,
     $               ', gamma =',E14.6,', CE =',E14.6)
        end if
      end if
C     !END
      call BYE ('TETUAN')
C
      return
      end
