      subroutine SMILO
     $(N,NL,NSL,RKI,CKI,SQS,SLT,GVL,KDGV,PIS)
C
C     Rudolf Loeser, 1978 Jun 27
C---- Computes PIS, for SETPIJ
C     !DASH
      save
C     !DASH
      real*8 CKI, GVL, PIS, RKI, SLT, SQS, SUM, XDEN, XNUM
      integer I, J, KDGV, N, NL, NSL
C     !DASH
      external ZERO1, DIVIDE, HI, BYE
C
C               RKI(N,NL), CKI(N,NL), GVL(N,NL), PIS(N,NL), SQS(N),
      dimension RKI(N,*),  CKI(N,*),  GVL(N,*),  PIS(N,*),  SQS(*),
C
C               SLT(N)
     $          SLT(*)
C
      call HI ('SMILO')
C     !BEG
      call ZERO1        (PIS,(N*NL))
C
      if(NSL.gt.NL) then
        do 101 J = 1,NL
C
          do 100 I = 1,N
            SUM = RKI(I,J)+CKI(I,J)
            if(KDGV.ne.0) then
              SUM = SUM+GVL(I,J)
            end if
            XNUM = SUM*SLT(I)
            XDEN = (SQS(I)+SLT(I))
            call DIVIDE (XNUM,XDEN,PIS(I,J))
  100     continue
C
  101   continue
      end if
C     !END
      call BYE ('SMILO')
C
      return
      end
