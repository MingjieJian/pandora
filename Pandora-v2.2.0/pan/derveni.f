      subroutine DERVENI
     $(N,NL,LCH,AMASS,TE,XNC,TEX,EQN,XNU,P,HN1,AIJ,JHACUL,KCHSW,CHIJ,
     $ FC,PRNT)
C
C     Rudolf Loeser, 1991 May 15
C---- Computes CHIJ for OPHIR,
C     for all transitions  with lower-level index not greater than LCH.
C
C     From:
C     Drawin 1969, Z.Physik, 225, 483.
C
C     (This is version 2 of DERVENI.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, AMASS, AUL, CHIJ, CUL, D, DNU, EQN, EQNL, EQNU, FAC,
     $       FC, H, HN1, P, RAT, TE, TEX, W, XNC, XNU, Y, ZERO
      integer I, ICHDP, IL, ILU, IU, IUL, JHACUL, KCHSW, LCH, LIM, N,
     $        NL
      logical DUMP, KILROY, PRNT
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(118),ICHDP)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  ECHUIR, INDXIJ, FAGNI, DIVIDE, EFE, ERSA, ETAR, MASHED,
     $          ZERO1, HI, BYE
      intrinsic min, abs
C
C               TE(N), TEX(N), FC(N), HN1(N), XNU(NSL), P(NSL), XNC(N),
      dimension TE(*), TEX(*), FC(*), HN1(*), XNU(*),   P(*),   XNC(*),
C
C               AIJ(NL,NL), CHIJ(N,NL**2), EQN(NL), KCHSW(NL,NL)
     $          AIJ(*),     CHIJ(N,*),     EQN(*),  KCHSW(NL,*)
C
      call HI ('DERVENI')
C     !BEG
      if(LCH.eq.0) then
        do 100 IU = 2,NL
          IL = 1
          call INDXIJ (IL, IU, ILU)
          call INDXIJ (IU, IL, IUL)
          call ZERO1  (CHIJ(1,ILU), N)
          call ZERO1  (CHIJ(1,IUL), N)
          KCHSW(IU,IL) = 2
          KCHSW(IL,IU) = 2
  100   continue
      else
C     !EJECT
        KILROY = .true.
C
        do 103 IU = 2,NL
          EQNU = EQN(IU)
          LIM  = min((IU-1),(abs(LCH)))
 
          do 102 IL = 1,LIM
            EQNL = EQN(IL)
            call ECHUIR     (N, IU, IL, NL, AIJ, AMASS, TE, XNC, HN1,
     $                       FC, AUL)
C
            if(AUL.gt.ZERO) then
              call EFE      (EQNL, EQNU, W, D)
              call INDXIJ   (IL, IU, ILU)
              call INDXIJ   (IU, IL, IUL)
              DNU = XNU(IU)-XNU(IL)
              do 101 I = 1,N
                call ERSA   (AMASS, TE(I), W, Y, H)
                DUMP = (I.eq.ICHDP).and.PRNT
                if(DUMP) then
                  call ETAR (KILROY, 'DERVENI', IU, IL, I, TE(I),
     $                       HN1(I), FC(I), AUL, AMASS, EQNL, EQNU,
     $                       Y, H, D, W)
                end if
                call FAGNI  (P(IL), P(IU), TEX(I), DNU, FAC, DUMP)
                call DIVIDE (H, D, RAT)
                CUL = FC(I)*RAT
C
                CHIJ(I,ILU) = CUL
                CHIJ(I,IUL) = CUL*FAC
  101         continue
              JHACUL = 1
            else
C             These have to be erased in case nonzero Kaulakys values
C             (subroutine DAMALA) were put there.
              call ZERO1    (CHIJ(1,ILU), N)
              call ZERO1    (CHIJ(1,IUL), N)
            end if
            KCHSW(IU,IL) = 2
            KCHSW(IL,IU) = 2
C
  102     continue
C
  103   continue
        if(.not.KILROY) then
          call MASHED       ('DERVENI')
        end if
C
      end if
C     !END
      call BYE ('DERVENI')
C
      return
      end
