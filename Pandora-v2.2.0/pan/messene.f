      subroutine MESSENE
     $(N,NL,XNU,ALF)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Computes Alpha, for each transition.
C     !DASH
      save
C     !DASH
      real*8 ALF, DNU, XNU
      integer IL, IU, IUL, N, NL
C     !DASH
      external INDXUL, VALVE, HI, BYE
C
C               XNU(NSL), ALF(MUL)
      dimension XNU(*),   ALF(*)
C
      call HI ('MESSENE')
C     !BEG
      do 101 IU = 2,NL
        do 100 IL = 1,(IU-1)
C
          DNU = XNU(IU)-XNU(IL)
          call INDXUL (IU, IL, IUL)
C
          call VALVE  (DNU, ALF(IUL))
C
  100   continue
  101 continue
C     !END
      call BYE ('MESSENE')
C
      return
      end
