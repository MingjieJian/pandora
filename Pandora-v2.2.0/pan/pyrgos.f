      subroutine PYRGOS
     $(N,NL,T,XNU,BATA)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Computes Stimulated Emission factors, for each transition.
C     (See also ZANCLE.)
C     !DASH
      save
C     !DASH
      real*8 BATA, DNU, HNUKT, T, XNU
      integer I, IL, IU, IUL, N, NL
C     !DASH
      external INDXUL, PROD, HI, BYE
C
C               T(N), XNU(NSL), BATA(N,MUL)
      dimension T(*), XNU(*),   BATA(N,*)
C
C
      call HI ('PYRGOS')
C     !BEG
      do 102 IU = 2,NL
        do 101 IL = 1,(IU-1)
          call INDXUL (IU, IL, IUL)
          DNU = XNU(IU)-XNU(IL)
          do 100 I = 1,N
            call PROD (T(I), DNU, 1, HNUKT, BATA(I,IUL))
  100     continue
  101   continue
  102 continue
C     !END
      call BYE ('PYRGOS')
C
      return
      end
