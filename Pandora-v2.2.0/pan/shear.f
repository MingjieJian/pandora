      subroutine SHEAR
     $(LU,LUG,FMULT,LYM,KISLV,TPOP,CBHS,CSB,BHSNUM,S1,SR,SS1,SSR,HEAD,
     $ BHSDEN,B,BHS,ISW,PLTID,NOPAC,N,GRAF)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Sets up Emitters printout, for SHARON.
C     !DASH
      save
C     !DASH
      real*8 B, BHS, BHSDEN, BHSNUM, CBHS, CSB, FMULT, S1, SR, SS1, SSR
      integer ISW, KISLV, KNFRM, KONFRM, LU, LUG, N, NOPAC
      logical GRAF, LYM
      character HEAD*12, PLTID*1, TPOP*3
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
      equivalence (KZQ( 26),KNFRM)
C     !DASH
C     !EJECT
      external HAVGAN, KRAAL, MOVE1, CRAWL, TWEET, HI, BYE
C
C               SR(N), CBHS(Nopac,N), CSB(Nopac,N), SSR(N), ISW(Nopac),
      dimension SR(*), CBHS(*),       CSB(*),       SSR(*), ISW(*),
C
C               BHSNUM(N), S1(N), SS1(N), BHS(N), BHSDEN(N), B(N)
     $          BHSNUM(*), S1(*), SS1(*), BHS(*), BHSDEN(*), B(*)
C
      dimension PLTID(4)
C
      call HI ('SHEAR')
C     !BEG
      GRAF = .false.
      if(LU.gt.0) then
        call HAVGAN    (BHSNUM, N, KNFRM, KONFRM)
        call MOVE1     (CBHS, (N*NOPAC), CSB)
C
        if(KONFRM.eq.1) then
          call TWEET   (N, NOPAC, HEAD, BHS, BHSNUM, BHSDEN, B, CSB,
     $                  KONFRM, LU, LUG, PLTID, S1, SR, LYM, TPOP,
     $                  KISLV, ISW)
C
        else if(KONFRM.eq.2) then
          call KRAAL   (CSB, BHSNUM, FMULT, N, NOPAC, KONFRM)
          call CRAWL   (LYM, S1, SR, SS1, SSR, BHSNUM, FMULT, N)
          call TWEET   (N, NOPAC, HEAD, BHS, BHSNUM, BHSDEN, B, CSB,
     $                  KONFRM, LU, LUG, PLTID, SS1, SSR, LYM, TPOP,
     $                  KISLV, ISW)
          GRAF = .true.
        end if
      end if
C     !END
      call BYE ('SHEAR')
C
      return
      end
