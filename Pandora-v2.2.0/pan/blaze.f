      subroutine BLAZE
     $(N,Z,R1N,MRR,FRR,K,SI,AS,SF,SFC,DI,AD,DF,DFC,TF)
C
C     Rudolf Loeser, 1981 Aug 25
C---- Computes Shell Flux (SF) from Shell Intensities (SI),
C     Disk Flux (DF) from Disk Intensities (DI), and Total Flux (TF).
C     (This is version 2 of BLAZE.)
C     !DASH
      save
C     !DASH
      real*8 AD, AS, DF, DFC, DI, FRR, R1N, SF, SFC, SI, TF, Z, ZERO
      integer I, J, K, MRR, N
      logical DISK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, TOBOSA, HI, BYE
C
C               SI(N,K), SF(K), DI(MRR,K), TF(K), SFC(N,K), DFC(MRR,K),
      dimension SI(N,*), SF(*), DI(MRR,*), TF(*), SFC(N,*), DFC(MRR,*),
C
C               DF(K), Z(N), FRR(MRR), AS(N), AD(MRR)
     $          DF(*), Z(*), FRR(*),   AS(*), AD(*)
C
C     !EJECT
C
      call HI ('BLAZE')
C     !BEG
      DISK = MRR.gt.0
C
C---- Compute flux profiles
      do 100 J = 1,K
C----   Shell
        call TOBOSA   (N  ,AS,SI(1,J),SFC(1,J),SF(J))
        if(DISK) then
C----     Disk
          call TOBOSA (MRR,AD,DI(1,J),DFC(1,J),DF(J))
        else
          DF(J) = ZERO
          call ZERO1  (DFC(1,J),MRR)
        end if
C----   Total
        TF(J) = SF(J)+DF(J)
  100 continue
C
C---- Compute cumulative contributions
      do 103 J = 1,K
        if(DISK) then
C----     Disk
          if(MRR.gt.1) then
            do 101 I = 2,MRR
              DFC(I,J) = DFC(I,J)+DFC(I-1,J)
  101       continue
          end if
        end if
C----   Shell
        SFC(N,J) = SFC(N,J)+DF(J)
        do 102 I = (N-1),1,-1
          SFC(I,J) = SFC(I,J)+SFC(I+1,J)
  102   continue
  103 continue
C     !END
      call BYE ('BLAZE')
C
      return
      end
