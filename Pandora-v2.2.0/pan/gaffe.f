      subroutine GAFFE
     $(N,K,A,DL,DW,DP,XJNU,DJB,LDL,DDL,FDDL,CDL,GII,VG,WG,GJN,XK2)
C
C     Rudolf Loeser, 2005 Feb 15
C---- Computes the PRD terms: GJN and K2.
C     (See also JAFFA.)
C     !DASH
      save
C     !DASH
      real*8 A, AG, CDL, DDL, DJB, DL, DP, DW, FDDL, GII, GJN, GJRAT,
     $       ONE, RJNU, SMDEN, SMNUM, TIME, VG, WG, XJNU, XK2, ZERO,
     $       dummy
      integer I, II, J, JJ, JP, K, L, LDL, N
      logical DMPI, DMPJ, DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external GRINCH, DIVIDE, BROCKEN, DOLFIN, MASHED, ARRADD, ARRMUL,
     $         ZAGEN, ZERO1, DRAT, HI, BYE
C
C               XJNU(N,K), XK2(N,K), GII(K,K), FDDL(N), A(K), DDL(LDL),
      dimension XJNU(N,*), XK2(N,*), GII(K,*), FDDL(*), A(*), DDL(*),
C
C               GJN(N,K), DJB(N,K), DL(K), CDL(LDL), DP(N,LDL), DW(N),
     $          GJN(N,*), DJB(N,*), DL(*), CDL(*),   DP(*),     DW(*),
C
C               VG(K), WG(K)
     $          VG(*), WG(*)
C
      call HI ('GAFFE')
C     !BEG
      call ZERO1   (GJN, (N*K))
      call BROCKEN (N, K, 'GAFFE', DUMP, II, JJ)
C     !EJECT
      do 103 L = 1,LDL
        do 102 I = 1,N
C
          DMPI = DUMP.and.(I.eq.II)
          call ZAGEN      (DMPI, L, CDL(L), I)
          call GRINCH     (L, I, N, K, DL, DDL, FDDL, DW, DP, VG, WG,
     $                     DMPI, GII, TIME)
          do 101 J = 1,K
            DMPJ = DMPI.and.(J.eq.JJ)
C
            SMNUM = ZERO
            SMDEN = ZERO
            do 100 JP = 1,K
              call DOLFIN (J, JP, A(JP), GII(JP,J), XJNU(I,JP), DMPJ)
              call DRAT   (XJNU(I,JP), XJNU(I,J), dummy, RJNU)
              AG    = A(JP)*GII(JP,J)
              SMDEN = SMDEN+AG
              SMNUM = SMNUM+AG*(RJNU-ONE)
  100       continue
C
            call DIVIDE   (SMNUM, SMDEN, GJRAT)
            GJN(I,J) = GJN(I,J)+CDL(L)*GJRAT
  101     continue
C
  102   continue
  103 continue
C
C
      do 104 J = 1,K
        call ARRMUL       (GJN(1,J), XJNU(1,J), XK2(1,J), N)
        call ARRADD       (XK2(1,J),  DJB(1,J), XK2(1,J), N)
  104 continue
C
      if(DUMP) then
        call MASHED       ('GAFFE')
      end if
C     !END
      call BYE ('GAFFE')
C
      return
      end
