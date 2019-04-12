      subroutine CHUVASH
     $(LU,QNAME,IU,IL,DISK,TOT,DL,K,R1N,Z,N,FRR,MRR,SI,DI,WVL,WVNUM)
C
C     Rudolf Loeser, 1981 Dec 10
C---- Saves "Eclipse" intensities.
C     !DASH
      save
C     !DASH
      real*8 DI, DL, FRR, R1N, SI, WVL, WVNUM, Z
      integer I, IL, IU, J, K, LU, MRR, N
      logical DISK
      character QNAME*8, TOT*(*)
C     !DASH
      external  HI, BYE
C
C               DL(K), Z(N), FRR(MRR), SI(N,K), DI(MRR,K), WVNUM(K)
      dimension DL(*), Z(*), FRR(*),   SI(N,*), DI(MRR,*), WVNUM(*)
C
      call HI ('CHUVASH')
C     !BEG
      write (LU,100) QNAME,IU,IL,TOT,K,N,MRR,R1N,WVL
  100 format('"Eclipse" intensity, ',A,', line (',I2,',',
     $       I2,'), ',A/
     $       3I5/
     $       5X,1PE15.7,E20.12)
      write (LU,101) (DL(I),I=1,K)
      write (LU,101) (WVNUM(I),I=1,K)
  101 format(5X,1P3E25.12)
C
      write (LU,102)   (Z(I),I=1,N)
      if(DISK) then
        write (LU,102) (FRR(I),I=1,MRR)
      end if
  102 format(5X,1P5E15.7)
C
      do 104 J = 1,K
        write (LU,103)   J,(SI(I,J),I=1,N)
        if(DISK) then
          write (LU,103) J,(DI(I,J),I=1,MRR)
        end if
  103   format(I5,1P5E15.7/(5X,5E15.7))
  104 continue
C     !END
      call BYE ('CHUVASH')
C
      return
      end
