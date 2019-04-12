      subroutine DRINK
     $(I,N,KP,K,L,DL,DDL,FDDL,DW,DP,TAUM,NDR,XDR,DDR,XC,XP,DRLIMI,
     $ DUMP,DR)
C
C     Rudolf Loeser, 1978 Apr 22
C---- Controls computation of DR, for PRD.
C     (This is version 2 of DRINK.)
C     !DASH
      save
C     !DASH
      real*8 DDL, DDR, DL, DP, DR, DRLIMI, DRS, DW, FDDL, TAUM, XC, XDR,
     $       XI, XIK, XIKP, XIS, XP, XXC
      integer I, K, KP, L, LUEO, N, NDR
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  DIVIDE, DEAR, LINER, HI, BYE
      intrinsic abs, max
C
C               DL(K), DW(N), DP(N,LDL), XDR(NDR), DDL(LDL), DRLIMI(N),
      dimension DL(*), DW(*), DP(N,*),   XDR(*),   DDL(*),   DRLIMI(*),
C
C               DDR(NDR), TAUM(N), FDDL(N)
     $          DDR(*),   TAUM(*), FDDL(*)
C
      data XIS,DRS,XXC /-1.D0,  1.D0, 0.D0/
C     !EJECT
C
      call HI ('DRINK')
C     !BEG
      call DIVIDE  ((DL(K )-DDL(L)*FDDL(I)), DW(I), XIK )
      call DIVIDE  ((DL(KP)-DDL(L)*FDDL(I)), DW(I), XIKP)
      XI = max(abs(XIK),abs(XIKP))
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) I,K,KP,L,DW(I),DP(I,L),TAUM(I),DDL(L),DL(K),
     $                   XIK,DL(KP),XIKP,XI
  100   format(' ','I=',I3,3X,'K=',I3,3X,'KP=',I3,3X,'L=',I3,1P,
     $             3X, '    DW=',E12.4,3X,'    DP=',E12.4,
     $             3X, '  TAUM=',E12.4,3X,'   DDL=',E12.4/
     $         ' ',33X,' DL(K)=',E12.4,3X,'   XIK=',E12.4,
     $             3X, 'DL(KP)=',E12.4,3X,'  XIKP=',E12.4/
     $         ' ',33X,'    XI=',E12.4)
      end if
C
      if(XIS.ne.XI) then
        XIS = XI
        call DEAR  (XDR, DDR, NDR, XC, XP, DRLIMI(I), DW(I), DP(I,L),
     $              TAUM(I), XXC, XIS, DRS)
      end if
      DR = DRS
C
      if(DUMP.gt.0) then
        write (LUEO,101) XP,XC,XXC,DRLIMI(I),DR
  101   format(' ',30X,1P,
     $             3X,'    XP=',E12.4,3X,'    XC=',E12.4,
     $             3X,'   XXC=',E12.4,3X,' DRLIM=',E12.4/
     $         ' ',96X,1P,            3X,'    DR=',E12.4)
      end if
C     !END
      call BYE ('DRINK')
C
      return
      end
