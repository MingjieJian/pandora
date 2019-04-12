      subroutine YURAK
     $(Z,FRR,XDSK,EMDSK,CDSK,WDSK,LFLX,W)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Computes geometrical quantities for Disk Rays.
C     !DASH
      save
C     !DASH
      real*8 CDSK, EMDSK, FRR, R1N, W, WDSK, XDSK, Z
      integer J, LFLX, MRR, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 23),R1N  )
C     !DASH
      external CLARUS, KALMYK, ORISSA, ARRMUL, HI, BYE
C
      dimension W(*)
C
C               FRR(MRR), XDSK(N,MRR), EMDSK(N,MRR), CDSK(N,MRR), Z(N),
      dimension FRR(*),   XDSK(N,*),   EMDSK(N,*),   CDSK(*),     Z(*),
C
C               WDSK(N,MRR)
     $          WDSK(*)
C
      call HI ('YURAK')
C     !BEG
      if(MRR.gt.0) then
        do 100 J = 1,MRR
          call CLARUS (N, Z, FRR(J), XDSK(1,J), W)
          call KALMYK (R1N, FRR(J), Z, N, EMDSK(1,J))
  100   continue
        call ORISSA   (N, MRR, EMDSK, CDSK)
        if(LFLX.gt.0) then
          call ARRMUL (EMDSK, CDSK, WDSK, (N*MRR))
        end if
      end if
C     !END
      call BYE ('YURAK')
C
      return
      end
