      subroutine EULE
     $(P,SAVE,PD,VEC,IPNT)
C
C     Rudolf Loeser, 1991 Dec 16
C---- Computes P, a value of the absorption profile
C     in the case of Hydrogen with Stark broadening.
C     (This is version 2 of EULE.)
C     !DASH
      save
C     !DASH
      real*4 CRT, EST, F, FF, G, GG, P, PD, PZERO, SAVE, TL, TU, VEC,
     $       XMAX, XMIN, ZERO
      integer I, IPNT, NT
      logical GOOD, PRNT
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
C     !DASH
      external COUSIN, EBER, ODEAR, EMMA, LINER, HI, BYE
C
C               SAVE(NMAX,4), PD(9,KMAX), VEC(NMAX), IPNT(NMAX)
      dimension SAVE(NMAX,*), PD(*),      VEC(*),    IPNT(*)
C
      dimension TL(3), TU(3)
C
      data ZERO /0.E0/
C
      call HI ('EULE')
C     !BEG
      PRNT = LU.gt.0
C---- Compute CSN
      call COUSIN
C---- Set up subintegrals
      call EBER        (XMIN, XMAX, TL, TU, NT, PZERO)
C     !EJECT
      if(NT.gt.0) then
C----   Compute complete integrals
        F = ZERO
        G = ZERO
        do 101 I = 1,NT
          if(PRNT) then
            call LINER (5, LU)
            write (LU,100) I,TL(I),TU(I)
  100       format(' ','Subintegral #',I2,', from',1PE14.6,' to',E14.6)
            NS = 0
          end if
C
          call ODEAR   (TL(I), TU(I), SAVE, FF, GG, PD, GOOD, EST, CRT)
          F = F+FF
          G = G+GG
C
          if(PRNT) then
            call EMMA  (LU, FF, GG, SAVE(1,1), SAVE(1,2), SAVE(1,3),
     $                  SAVE(1,4), VEC, IPNT, NS, NMAX, GOOD, SBFQ,
     $                  SMSK, EST, CRT)
          end if
C
  101   continue
        if(G.eq.ZERO) then
          P = ZERO
        else
          P = F/G
        end if
C
        if(PRNT) then
          call LINER   (1, LU)
          write (LU,102) F,G,P
  102     format(' ','********** Final value of absorption profile:',
     $               1PE14.6,' /',E14.6,' =',E14.6)
        end if
      else
C----   Use average Voigt function value
        P = PZERO
      end if
C     !END
      call BYE ('EULE')
C
      return
      end
