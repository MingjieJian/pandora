      subroutine CRESS
     $(N,YA,WN,KODE,CNXP,RR)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes auxiliary function RR, for CRANE.
C     !DASH
      save
C     !DASH
      real*8 CNXP, RR, WN, YA
      integer I, KODE, N
C     !DASH
      external ZERO1, SUMPROD, ARRADD, HI, BYE
C
C               YA(N), CNXP(N), RR(N), WN(N,N)
      dimension YA(*), CNXP(*), RR(*), WN(N,*)
C
      call HI ('CRESS')
C     !BEG
      call ZERO1       (RR,N)
C
      if(KODE.eq.1) then
        do 100 I = 1,N
          call SUMPROD (RR(I),WN(I,1),N,YA,1,N)
  100   continue
      end if
C
      call ARRADD      (RR,YA  ,RR,N)
      call ARRADD      (RR,CNXP,RR,N)
C     !END
      call BYE ('CRESS')
C
      return
      end
