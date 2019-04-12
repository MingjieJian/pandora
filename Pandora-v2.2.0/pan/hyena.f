      subroutine HYENA
     $(WS,SNU,N,YNT,MUX,YY,KODE)
C
C     Rudolf Loeser, 2000 Jul 13
C---- Computes the intensity integral, for SIMBA.
C     (This is version 3 of HYENA.)
C     !DASH
      save
C     !DASH
      real*8 SNU, WS, WSNU, YH, YL, YNT, YY, ZERO
      integer I, KODE, MUX, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, VIPER, HI, BYE
C
C               WS(N), SNU(N)
      dimension WS(*), SNU(*)
C     !EJECT
C
      call HI ('HYENA')
C     !BEG
C---- Initialize the sum
      YNT = WS(1)*SNU(1)
      MUX = 1
      YY  = YNT
C
C---- Compute and add remaining terms
      do 100 I = 2,N
C
        if(WS(I).ne.ZERO) then
          WSNU = WS(I)*SNU(I)
          YNT  = YNT+WSNU
          if(YY.lt.WSNU) then
            YY  = WSNU
            MUX = I
          end if
        end if
C
  100 continue
C
C---- Compute fraction of total from greatest component term
C     and the two neighboring ones
      if(MUX.le.1) then
        YL = ZERO
      else
        YL = WS(MUX-1)*SNU(MUX-1)
      end if
      if(MUX.ge.N) then
        YH = ZERO
      else
        YH = WS(MUX+1)*SNU(MUX+1)
      end if
      call DIVIDE ((YL+YY+YH),YNT,YY)
C
C---- Verify the computed integral (if requested)
      call VIPER  (YNT,SNU,N,KODE)
C     !END
      call BYE ('HYENA')
C
      return
      end
