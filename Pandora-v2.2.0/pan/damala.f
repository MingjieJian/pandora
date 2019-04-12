      subroutine DAMALA
     $(N,NL,AMASS,TE,TEX,LCH,EQN,XNU,P,HN1,JHACUL,KCHSW,CHIJ,WC,FC,
     $ NINT,XTAB,FTAB,PRNT)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes CHIJ for OPHIR, for all transitions between levels that
C     both have LCH > 0.
C
C     From:
C
C     Kaulakys, B., J.Phys.B, 18, L167, (1985).
C
C     Sets JHACUL=1 if any, =0 if none.
C     !DASH
      save
C     !DASH
      real*8 AMASS, CHIJ, CUL, DNU, EQN, EQNL, EQNU, FAC, FC, FMASS,
     $       FTAB, HN1, ONE, P, SIGMA, TE, TEX, WC, XLIM, XNU, XTAB,
     $       dummy
      integer I, ICHDP, IL, ILU, IU, IUL, JHACUL, KCHSW, LCH, N, NINT,
     $        NL
      logical DUMP, JILROY, KILROY, PRNT
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ESTIMO, INDXIJ, DAMPIER, FOLCHI, FAGNI, MASHED, BERNICE,
     $         HI, BYE
C
C               TEX(N), TE(N), LCH(NSL), HN1(N), CHIJ(N,NL**2), P(NSL),
      dimension TEX(*), TE(*), LCH(*),   HN1(*), CHIJ(N,*),     P(*),
C
C               FTAB(NINT), WC(N,NL), EQN(NL), XNU(NSL), KCHSW(NL,NL),
     $          FTAB(*),    WC(N,*),  EQN(*),  XNU(*),   KCHSW(NL,*),
C
C               FC(N), XTAB(NINT)
     $          FC(*), XTAB(*)
C     !EJECT
C
      call HI ('DAMALA')
C     !BEG
      JILROY = .true.
      KILROY = .true.
      FMASS  = (AMASS/(AMASS+ONE))
C
      do 102 IU = 3,NL
        EQNU = EQN(IU)
        if(LCH(IU).gt.0) then
          do 101 IL = 2,(IU-1)
            EQNL = EQN(IL)
            if(LCH(IL).gt.0) then
              call ESTIMO      (N, FMASS, HN1, TE, FC, JILROY)
              call INDXIJ      (IU, IL, IUL)
              call INDXIJ      (IL, IU, ILU)
              DNU = XNU(IU)-XNU(IL)
C
              do 100 I=1,N
                DUMP = (I.eq.ICHDP).and.PRNT
                if(DUMP) then
                  call DAMPIER (KILROY, 'DAMALA', AMASS, I, TE(I),
     $                          HN1(I), IU, EQNU, IL, EQNL)
                end if
                call BERNICE   (IU, IL, XNU, dummy, TE(I), XLIM)
                call FOLCHI    (EQNL, EQNU, WC(I,IL), SIGMA, XLIM,
     $                          XTAB, FTAB, NINT, DUMP)
                call FAGNI     (P(IL), P(IU), TEX(I), DNU, FAC,
     $                          DUMP)
                CUL = (FC(I)/(EQNU**3))*SIGMA
                CHIJ(I,ILU) = CUL
                CHIJ(I,IUL) = CUL*FAC
  100         continue
              JHACUL = 1
C
              KCHSW(IU,IL) = 1
              KCHSW(IL,IU) = 1
            end if
  101     continue
        end if
  102 continue
      if(.not.KILROY) then
        call MASHED            ('DAMALA')
      end if
C     !END
      call BYE ('DAMALA')
C
      return
      end
