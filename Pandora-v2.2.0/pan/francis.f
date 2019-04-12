      subroutine FRANCIS
     $(N,NL,AMASS,TE,LCH,EQN,XNU,XNUC,HN1,JHACL,CHKI,WC,FC,NINT,
     $ XTAB,FTAB,PRNT)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes CHKI for OPHIR, for levels 2 and higher that
C     have LCH > 0.
C
C     From:
C
C     Kaulakys, B., J.Phys.B, 18, L167 (1985)
C
C     Sets JHACL=1 if any, =0 if none.
C     !DASH
      save
C     !DASH
      real*8 AMASS, CHKI, EQN, FC, FTAB, HN1, ONE, RMASS, SIGMA, TE, WC,
     $       XLIM, XNU, XNUC, XTAB
      integer I, ICHDP, J, JHACL, LCH, N, NINT, NL
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
C     !EJECT
      external ELATHA, DARLING, FOPPA, MASHED, BERNICE, HI, BYE
C
C               WC(N,NL), TE(N), LCH(NSL), FC(N), CHKI(N,NSL), EQN(NL),
      dimension WC(N,*),  TE(*), LCH(*),   FC(*), CHKI(N,*),   EQN(*),
C
C               HN1(N), XTAB(NINT), FTAB(NINT), XNU(NSL), XNUC(NSL)
     $          HN1(*), XTAB(*),    FTAB(*),    XNU(*),   XNUC(*)
C
      call HI ('FRANCIS')
C     !BEG
      RMASS = sqrt(AMASS/(AMASS+ONE))
C
      JILROY = .true.
      KILROY = .true.
      do 101 J = 2,NL
        if(LCH(J).gt.0) then
          call ELATHA      (N, RMASS, HN1, FC, JILROY)
C
          do 100 I = 1,N
            DUMP = (I.eq.ICHDP).and.PRNT
            if(DUMP) then
              call DARLING (KILROY, 'FRANCIS', AMASS, I, J, TE(I),
     $                      HN1(I), EQN(J))
            end if
            call BERNICE   (J, 0, XNU, XNUC, TE(I), XLIM)
            call FOPPA     (WC(I,J), SIGMA, XLIM, XTAB, FTAB, NINT,
     $                      DUMP)
            CHKI(I,J) = (FC(I)/EQN(J))*SIGMA
  100     continue
C
          JHACL  = 1
        end if
  101 continue
      if(.not.KILROY) then
        call MASHED        ('FRANCIS')
      end if
C     !END
      call BYE ('FRANCIS')
C
      return
      end
