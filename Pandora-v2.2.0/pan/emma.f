      subroutine EMMA
     $(LU,FF,GG,X,U,PHI,ELL,VEC,IPNT,NS,NMAX,GOOD,SBFQ,SMSK,EST,CRT)
C
C     Rudolf Loeser, 1991 Dec 18
C---- Prints detailed output, for EULE.
C     (This is version 2 of EMMA.)
C     !DASH
      save
C     !DASH
      real*4 CRT, ELL, EST, FF, GG, PHI, SBFQ, SMSK, U, VEC, X
      integer I, IPNT, L, LU, M, NMAX, NS
      logical GOOD
C     !DASH
      external  SINGR, ORDERR, LINER, EMORY, HI, BYE
      intrinsic min
C
C               IPNT(NMAX), U(NMAX), PHI(NMAX), ELL(NMAX), VEC(NMAX),
      dimension IPNT(*),    U(*),    PHI(*),    ELL(*),    VEC(*),
C
C               X(NMAX)
     $          X(*)
C
      call HI ('EMMA')
C     !BEG
      if(LU.gt.0) then
        M = min(NS,NMAX)
        call SINGR   (X,M,L,IPNT)
        call ORDERR  (U  ,IPNT,M,VEC)
        call ORDERR  (PHI,IPNT,M,VEC)
        call ORDERR  (ELL,IPNT,M,VEC)
C
        call LINER   (1,LU)
        write (LU,100) X(1),X(M),FF,GG,GOOD,L,NS,NMAX,SBFQ,SMSK,EST,CRT
  100   format(' ','Partial integrals from X =',1PE14.6,' to X =',
     $             E14.6,', F =',E14.6,' and G =',E14.6//
     $         ' ','GOOD =',L2,', L =',I2,', NS =',I6,', NMAX =',I6/
     $         ' ','HSBFEQ (accuracy) =',E10.2,5X,'MSK =',E10.2,5X,
     $             'est =',E12.4,5X,'CRT =',E10.2//
     $         ' ',18X,'X',18X,'U',9X,'Voigt',16X,'Ell',10X,
     $             'Voigt*Ell')
        call LINER   (1,LU)
        write (LU,101) (I,X(I),U(I),PHI(I),ELL(I),(PHI(I)*ELL(I)),
     $                  I=1,M)
  101   format(5(' ',I5,1PE14.6,5X,2E14.6,2E19.6/))
C
        if(L.gt.0) then
C----     Plot
          call EMORY (LU,X,PHI,ELL,VEC,M)
        end if
      end if
C     !END
      call BYE ('EMMA')
C
      return
      end
