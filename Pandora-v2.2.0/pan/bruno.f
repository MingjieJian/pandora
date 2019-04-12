      subroutine BRUNO
     $(NW,N,DISK,MRR,R1N,WAVES,SIS,SES,SFS,DIS,F,Z,FRR,NO)
C
C     Rudolf Loeser, 1993 Feb 19
C---- Computes and prints "averages", for RAJA.
C     Note: each "average" is stored in column 1 of the corresponding
C     array.
C     !DASH
      save
C     !DASH
      real*8 DIS, F, FRR, R1N, SES, SFS, SIS, WAVES, Z
      integer I, MRR, N, NO, NW
      logical DISK
C     !DASH
C     !EJECT
      external MOVED, BARRY, RONBU, LINER, HI, BYE
C
C               WAVES(Nmkuse), SIS(N,Nmkuse), SES(N,Nmkuse), FRR(MRR),
      dimension WAVES(*),      SIS(N,*),      SES(N,*),      FRR(*),
C
C               SFS(N,Nmkuse), DIS(MRR,Nmkuse), Z(N), F(max(N,MRR))
     $          SFS(N,*),      DIS(MRR,*),      Z(*), F(*)
C
      call HI ('BRUNO')
C     !BEG
      do 100 I = 1,N
        call MOVED   (SIS(I,1), N, NW, F, 1, NW)
        call BARRY   (2, WAVES, F, NW, SIS(I,1))
        call MOVED   (SES(I,1), N, NW, F, 1, NW)
        call BARRY   (2, WAVES, F, NW, SES(I,1))
        call MOVED   (SFS(I,1), N, NW, F, 1, NW)
        call BARRY   (2, WAVES, F, NW, SFS(I,1))
  100 continue
C
      if(DISK) then
        do 101 I = 1,MRR
          call MOVED (DIS(I,1), MRR, NW, F, 1, NW)
          call BARRY (2, WAVES, F, NW, DIS(I,1))
  101   continue
      end if
C
      if(NO.gt.0) then
C----   Print
        call LINER   (4, NO)
        write (NO,102)
  102   format(' ','Averages'///
     $         ' ',12X,'Z',14X,'I',14X,'E',14X,'F')
        call LINER   (1, NO)
        write (NO,103) (I,Z(I),SIS(I,1),SES(I,1),SFS(I,1),I=1,N)
  103   format(5(' ',I3,1P4E15.4/))
C
        if(DISK) then
          call LINER (2, NO)
          write (NO,104)
  104     format(' ',10X,'FRR',13X,'I')
          call LINER (1, NO)
          write (NO,105) (I,FRR(I),DIS(I,1),I=1,MRR)
  105     format(5(' ',I3,1P2E15.4/))
        end if
      end if
C
C---- Save values in restart file
      call RONBU     (N, MRR, R1N, Z, SIS, SES, SFS, DISK, FRR, DIS)
C     !END
      call BYE ('BRUNO')
C
      return
      end
