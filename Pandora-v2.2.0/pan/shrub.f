      subroutine SHRUB
     $(IU,IL,M,K,WVL,EPC,KODE,EPS,DL,XI,Z)
C
C     Rudolf Loeser, 23 May 68
C---- Computes Integrated Intensities, for LISA.
C     KODE = 1 when "XI" = SI (shell);
C     KODE = 0 when "XI" = DI (disk).
C     !DASH
      save
C     !DASH
      real*8 CON, DEL, DL, DXNU, EPC, EPS, FAC, WVL, XI, Z
      integer I, IL, IU, J, K, KODE, M
C     !DASH
      external  ZERO1, ANGIE, SARACEN, RIGEL, CONMUL, HI, BYE
      intrinsic abs
C
C               m = N if shell
C                 = MRR if disk
C
C               EPC(m), DL(K), XI(m,K), EPS(N), Z(N)
      dimension EPC(*), DL(*), XI(M,*), EPS(*), Z(*)
C
      call HI ('SHRUB')
C     !BEG
      if(M.gt.0) then
        call ZERO1     (EPC,M)
C
        do 101 J = 2,K
          DEL = abs(DL(J)-DL(J-1))
          do 100 I = 1,M
            EPC(I) = EPC(I)+(XI(I,J)+XI(I,J-1))*DEL
  100     continue
  101   continue
C
        call ANGIE     (WVL,DXNU)
        call RIGEL     (34,CON)
        FAC = CON*(DXNU**2)
        call CONMUL    (FAC,EPC,M)
C
        if(KODE.eq.1) then
          call SARACEN (M,Z,EPC,EPS)
        end if
      end if
C     !END
      call BYE ('SHRUB')
C
      return
      end
