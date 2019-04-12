      subroutine BERTRAM
     $(J,N,DISK,MRR, SI,SE,SF,DI, SIS,SES,SFS,DIS)
C
C     Rudolf Loeser, 1993 Feb 19
C---- Saves data for "average continuum" quantities, for RAJA.
C     !DASH
      save
C     !DASH
      real*8 DI, DIS, SE, SES, SF, SFS, SI, SIS
      integer J, MRR, N
      logical DISK
C     !DASH
      external MOVE1, HI, BYE
C
C               SF(N), DI(MRR), DIS(MRR,Numkon), SFS(N,Numkon), SE(N),
      dimension SF(*), DI(*),   DIS(MRR,*),      SFS(N,*),      SE(*),
C
C               SIS(N,Numkon), SES(N,Numkon), SI(N)
     $          SIS(N,*),      SES(N,*),      SI(*)
C
      call HI ('BERTRAM')
C     !BEG
      call MOVE1   (SI,N  ,SIS(1,J))
      call MOVE1   (SE,N  ,SES(1,J))
      call MOVE1   (SF,N  ,SFS(1,J))
C
      if(DISK) then
        call MOVE1 (DI,MRR,DIS(1,J))
      end if
C     !END
      call BYE ('BERTRAM')
C
      return
      end
