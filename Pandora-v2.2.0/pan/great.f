      subroutine GREAT
     $(T,N,Y,IQFIN,IQREF,G)
C
C     Rudolf Loeser, 1971 Sep 21
C---- Computes the G matrix.
C     !DASH
      save
C     !DASH
      real*8 G, T, TJ, TN, Y
      integer I, IQFIN, IQREF, J, N
      logical IFN, IRF
C     !DASH
      external NINLIL, NIPPUR, NISAN, HI, BYE
C
C               T(N), G(N,N)
      dimension T(*), G(N,*)
C
C
      call HI ('GREAT')
C     !BEG
      IFN = IQFIN.gt.0
      IRF = IQREF.gt.0
      TN  = T(N)
C---- First column
        do 100 I = 1,N
          call NINLIL (T(I),TN,IRF,IFN,G(I,1))
  100   continue
C---- Interior columns
      do 102 J = 2,(N-1)
        TJ = T(J)
        do 101 I = 1,N
          call NIPPUR (T(I),TJ,TN,Y,IRF,G(I,J))
  101   continue
  102 continue
C---- Last column
        do 103 I = 1,N
          call NISAN  (T(I),TN,IRF,IFN,G(I,N))
  103   continue
C     !END
      call BYE ('GREAT')
C
      return
      end
